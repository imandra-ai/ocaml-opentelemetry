open Opentelemetry_atomic

type 'a state = {
  start: Mtime.t;
  size: int;
  q: 'a list;  (** The queue is a FIFO represented as a list in reverse order *)
}

type 'a t = {
  st: 'a state Atomic.t;
  batch: int;  (** Minimum size to batch before popping *)
  high_watermark: int;  (** Size above which we start dropping signals *)
  timeout: Mtime.span option;
}

let default_high_watermark batch_size = min 10 (max (batch_size * 10) 1_000_000)

let _dummy_start = Mtime.min_stamp

let _empty_state : _ state = { q = []; size = 0; start = _dummy_start }

let make ?(batch = 100) ?high_watermark ?now ?timeout () : _ t =
  let high_watermark =
    match high_watermark with
    | Some x -> x
    | None -> default_high_watermark batch
  in
  let start =
    match now with
    | Some x -> x
    | None -> _dummy_start
  in
  assert (batch > 0);
  {
    st = Atomic.make @@ { size = 0; q = []; start };
    batch;
    timeout;
    high_watermark;
  }

let timeout_expired_ ~now ~timeout (self : _ state) : bool =
  match timeout with
  | Some t ->
    let elapsed = Mtime.span now self.start in
    Mtime.Span.compare elapsed t >= 0
  | None -> false

(** Big enough to send? *)
let[@inline] is_full_ ~batch (self : _ state) : bool = self.size >= batch

let pop_if_ready ?(force = false) ~now (self : _ t) : _ list option =
  let rev_batch_opt =
    (* update state. When uncontended this runs only once. *)
    Util_atomic.update_cas self.st @@ fun state ->
    (* *)

    (* check if the batch is ready *)
    let ready_to_pop =
      state.size > 0
      && (force
         || is_full_ ~batch:self.batch state
         || timeout_expired_ ~now ~timeout:self.timeout state)
    in

    if ready_to_pop then (
      assert (state.q <> []);
      let batch = state.q in
      let new_st = _empty_state in
      Some batch, new_st
    ) else
      None, state
  in
  match rev_batch_opt with
  | None -> None
  | Some batch ->
    (* Reverse the list to retrieve the FIFO order. *)
    Some (List.rev batch)

let push (self : _ t) elems : [ `Dropped | `Ok ] =
  if elems = [] then
    `Ok
  else (
    let now = lazy (Mtime_clock.now ()) in
    Util_atomic.update_cas self.st @@ fun state ->
    if state.size >= self.high_watermark then
      (* drop this to prevent queue from growing too fast *)
      `Dropped, state
    else (
      let start =
        if state.size = 0 && Option.is_some self.timeout then
          Lazy.force now
        else
          state.start
      in

      (* add to queue *)
      let state =
        {
          size = state.size + List.length elems;
          q = List.rev_append elems state.q;
          start;
        }
      in

      `Ok, state
    )
  )

let[@inline] push' self elems = ignore (push self elems : [ `Dropped | `Ok ])

open Opentelemetry_emitter

let wrap_emitter (self : _ t) (e : _ Emitter.t) : _ Emitter.t =
  (* we need to be able to close this emitter before we close [e]. This
     will become [true] when we close, then we call [Emitter.flush_and_close e],
     then [e] itself will be closed. *)
  let closed_here = Atomic.make false in

  let enabled () = (not (Atomic.get closed_here)) && e.enabled () in
  let closed () = Atomic.get closed_here || e.closed () in
  let flush_and_close () =
    if not (Atomic.exchange closed_here true) then (
      (* NOTE: we need to close this wrapping emitter first, to prevent
         further pushes; then write the content to [e]; then
         flusn and close [e]. In this order. *)
      (match pop_if_ready self ~force:true ~now:Mtime.max_stamp with
      | None -> ()
      | Some l -> Emitter.emit e l);

      (* now we can close [e], nothing remains in [self] *)
      Emitter.flush_and_close e
    )
  in

  let maybe_emit ~now =
    if not (Atomic.get closed_here) then (
      match pop_if_ready self ~force:false ~now with
      | None -> ()
      | Some l -> Emitter.emit e l
    )
  in

  let tick ~now =
    (* first, check if batch has timed out *)
    maybe_emit ~now;

    (* only then, tick the underlying emitter *)
    Emitter.tick e ~now
  in

  let emit l =
    if l <> [] && (not (Atomic.get closed_here)) && e.enabled () then (
      push' self l;

      (* TODO: it'd be nice if we checked only for size here, not
        for timeout. The [tick] function is enough for timeouts,
        whereas [emit] is in the hot path of every single span/metric/log *)
      let now = Mtime_clock.now () in
      maybe_emit ~now
    )
  in

  { Emitter.closed; enabled; flush_and_close; tick; emit }
