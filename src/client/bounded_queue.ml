(** Interface for a thread-safe, bounded queue.

    After the high watermark is reached, pushing items into the queue will
    instead discard them. *)

open Common_

exception Closed
(** Raised when pushing into a closed queue *)

type 'a pop_result =
  [ `Empty
  | `Closed
  | `Item of 'a
  ]

type 'a t = {
  push: 'a list -> unit;
      (** Push items. This might discard some of them.
          @raise Closed if the queue is closed. *)
  num_discarded: unit -> int;  (** How many items were discarded? *)
  on_non_empty: (unit -> unit) -> unit;
      (** [on_non_empty f] registers [f] to be called whenever the queue
          transitions from empty to non-empty. *)
  try_pop: unit -> 'a pop_result;
      (** Try to pop an item right now. @raise Closed if the *)
  close: unit -> unit;
  closed: unit -> bool;
}

let[@inline] push (self : _ t) x : unit = self.push x

let[@inline] num_discarded self = self.num_discarded ()

let[@inline] try_pop (self : _ t) : _ pop_result = self.try_pop ()

let[@inline] on_non_empty (self : _ t) f = self.on_non_empty f

let[@inline] close (self : _ t) : unit = self.close ()

let[@inline] closed (self : _ t) : bool = self.closed ()

let to_emitter (self : 'a t) : 'a Opentelemetry_emitter.Emitter.t =
  let closed () = self.closed () in
  let enabled () = not (closed ()) in
  let emit x = if x <> [] then push self x in
  let tick ~now:_ = () in
  let flush_and_close () = close self in
  { closed; enabled; emit; tick; flush_and_close }
