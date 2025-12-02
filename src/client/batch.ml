module A = Opentelemetry_atomic.Atomic
module Domain = Opentelemetry_domain

type 'a state = {
  start: Mtime.t;
  size: int;
  q: 'a list;  (** The queue is a FIFO represented as a list in reverse order *)
}

type 'a t = {
  st: 'a state A.t;
  batch: int;  (** Minimum size to batch before popping *)
  high_watermark: int;  (** Size above which we start dropping signals *)
  timeout: Mtime.span option;
}

let default_high_watermark batch_size =
  if batch_size = 1 then
    100
  else
    batch_size * 10

let _dummy_start = Mtime.min_stamp

let _empty_state : _ state = { q = []; size = 0; start = _dummy_start }

let make ?(batch = 1) ?high_watermark ?now ?timeout () : _ t =
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
  { st = A.make { size = 0; q = []; start }; batch; timeout; high_watermark }

let timeout_expired_ ~now ~timeout (self : _ state) : bool =
  match timeout with
  | Some t ->
    let elapsed = Mtime.span now self.start in
    Mtime.Span.compare elapsed t >= 0
  | None -> false

(* Big enough to send a batch *)
let[@inline] is_full_ ~batch (self : _ state) : bool = self.size >= batch

let[@inline] atomic_update_loop_ (type res) (self : _ t)
    (f : 'a state -> 'a state * res) : res =
  let exception Return of res in
  try
    let backoff = ref 1 in
    while true do
      let st = A.get self.st in
      let new_st, res = f st in
      if A.compare_and_set self.st st new_st then raise_notrace (Return res);

      (* poor man's backoff strategy *)
      Domain.relax_loop !backoff;
      backoff := min 128 (2 * !backoff)
    done
  with Return res -> res

let pop_if_ready ?(force = false) ~now (self : _ t) : _ list option =
  let rev_batch_opt =
    (* update state. When uncontended this runs only once. *)
    atomic_update_loop_ self @@ fun state ->
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
      new_st, Some batch
    ) else
      state, None
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
    atomic_update_loop_ self @@ fun state ->
    if state.size >= self.high_watermark then
      (* drop this to prevent queue from growing too fast *)
      state, `Dropped
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

      state, `Ok
    )
  )
