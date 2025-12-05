module BQ = Bounded_queue

type 'a state = {
  n_discarded: int Atomic.t;
  high_watermark: int;
  q: 'a Sync_queue.t;
  on_non_empty: Cb_set.t;
}

let push (self : _ state) x =
  let discarded, old_size =
    try
      Sync_queue.push_while_not_full self.q ~high_watermark:self.high_watermark
        x
    with Sync_queue.Closed -> raise BQ.Closed
  in

  if discarded > 0 then
    ignore (Atomic.fetch_and_add self.n_discarded discarded : int);

  (* wake up lagards if the queue was empty *)
  if old_size = 0 then Cb_set.trigger self.on_non_empty;
  ()

let try_pop (self : _ state) : _ BQ.pop_result =
  match Sync_queue.try_pop self.q with
  | Some x -> `Item x
  | None -> `Empty
  | exception Sync_queue.Closed -> `Closed

let to_bounded_queue (self : 'a state) : 'a BQ.t =
  let closed () = Sync_queue.closed self.q in
  let num_discarded () = Atomic.get self.n_discarded in
  let push x = push self x in
  let on_non_empty = Cb_set.register self.on_non_empty in
  let try_pop () = try_pop self in
  let close () = Sync_queue.close self.q in
  { BQ.push; num_discarded; try_pop; on_non_empty; close; closed }

let create ~high_watermark () : _ BQ.t =
  let st =
    {
      high_watermark;
      q = Sync_queue.create ();
      n_discarded = Atomic.make 0;
      on_non_empty = Cb_set.create ();
    }
  in
  to_bounded_queue st

