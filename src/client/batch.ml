module A = Opentelemetry_atomic.Atomic

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

let[@inline] ready_to_pop_ ~force ~now ~batch ~timeout (self : _ state) =
  self.size > 0
  && (force || is_full_ ~batch self || timeout_expired_ ~now ~timeout self)

let[@inline] atomic_update_loop_ (type res) (self : _ t)
    (f : 'a state -> 'a state * res) : res =
  let exception Return of res in
  try
    while true do
      let st = A.get self.st in
      let new_st, res = f st in
      if A.compare_and_set self.st st new_st then raise_notrace (Return res)
    done
  with Return res -> res

let pop_if_ready ?(force = false) ~now (self : _ t) : _ list option =
  let rev_batch_opt =
    atomic_update_loop_ self @@ fun state ->
    let timeout = self.timeout in
    let batch = self.batch in
    if ready_to_pop_ ~force ~now ~batch ~timeout state then (
      assert (state.q <> []);
      let batch = state.q in
      let new_st = { q = []; size = 0; start = _dummy_start } in
      new_st, Some batch
    ) else
      state, None
  in
  match rev_batch_opt with
  | None -> None
  | Some batch ->
    (* Reverse the list to retrieve the FIFO order. *)
    Some (List.rev batch)

let[@inline] push_unprotected_ (self : _ state) (elems : _ list) : _ state =
  {
    self with
    size = self.size + List.length elems;
    q = List.rev_append elems self.q;
  }

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
      let state =
        if state.size = 0 && Option.is_some self.timeout then
          (* current batch starts now *)
          { state with start = Lazy.force now }
        else
          state
      in

      (* add to queue *)
      let state = push_unprotected_ state elems in

      state, `Ok
    )
  )
