module BQ = Bounded_queue

exception Closed = Bounded_queue.Closed

(* a variant of {!Sync_queue} with more bespoke pushing behavior *)
module Q : sig
  type 'a t

  val create : unit -> 'a t

  val close : _ t -> unit

  val closed : _ t -> bool

  val try_pop : 'a t -> 'a option

  val push_while_not_full : high_watermark:int -> 'a t -> 'a list -> int * int
  (** [push_while_not_full q ~high_watermark xs] tries to push each item of [x]
      into [q].

      An item is not pushed if the queue is "full" (size >= high_watermark).

      This returns a pair [num_discarded, old_size] where [num_discarded] is the
      number of items that could not be pushed, and [old_size] is the size
      before anything was pushed. *)
end = struct
  module UM = Opentelemetry_util.Util_mutex

  type 'a t = {
    mutex: Mutex.t;
    q: 'a Queue.t;
    mutable closed: bool;
  }

  let create () : _ t =
    { mutex = Mutex.create (); q = Queue.create (); closed = false }

  (* NOTE: the race condition here is benign, assuming no tearing of
    a value of type [bool] which OCaml's memory model should guarantee. *)
  let[@inline] closed self = self.closed

  let close (self : _ t) =
    UM.protect self.mutex @@ fun () ->
    if not self.closed then self.closed <- true

  let try_pop (self : 'a t) : 'a option =
    UM.protect self.mutex @@ fun () ->
    if self.closed then raise Closed;
    try Some (Queue.pop self.q) with Queue.Empty -> None

  let push_while_not_full ~high_watermark (self : 'a t) (xs : 'a list) :
      int * int =
    UM.protect self.mutex @@ fun () ->
    if self.closed then raise Closed;

    let old_size = Queue.length self.q in
    let xs = ref xs in

    let continue = ref true in
    while !continue && Queue.length self.q < high_watermark do
      match !xs with
      | [] -> continue := false
      | x :: tl_xs ->
        xs := tl_xs;
        Queue.push x self.q
    done;

    let n_discarded = List.length !xs in
    n_discarded, old_size
end

type 'a state = {
  n_discarded: int Atomic.t;
  high_watermark: int;
  q: 'a Q.t;
  on_non_empty: Cb_set.t;
}

let push (self : _ state) x =
  let discarded, old_size =
    try Q.push_while_not_full self.q ~high_watermark:self.high_watermark x
    with Sync_queue.Closed -> raise BQ.Closed
  in

  if discarded > 0 then
    ignore (Atomic.fetch_and_add self.n_discarded discarded : int);

  (* wake up lagards if the queue was empty *)
  if old_size = 0 then Cb_set.trigger self.on_non_empty;
  ()

let try_pop (self : _ state) : _ BQ.pop_result =
  match Q.try_pop self.q with
  | Some x -> `Item x
  | None -> `Empty
  | exception Sync_queue.Closed -> `Closed

let to_bounded_queue (self : 'a state) : 'a BQ.t =
  let closed () = Q.closed self.q in
  let num_discarded () = Atomic.get self.n_discarded in
  let push x = push self x in
  let on_non_empty = Cb_set.register self.on_non_empty in
  let try_pop () = try_pop self in
  let close () = Q.close self.q in
  { BQ.push; num_discarded; try_pop; on_non_empty; close; closed }

let create ~high_watermark () : _ BQ.t =
  let st =
    {
      high_watermark;
      q = Q.create ();
      n_discarded = Atomic.make 0;
      on_non_empty = Cb_set.create ();
    }
  in
  to_bounded_queue st
