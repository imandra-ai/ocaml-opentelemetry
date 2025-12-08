(** Interface for a thread-safe, bounded queue.

    After the high watermark is reached, pushing items into the queue will
    instead discard them. *)

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
  try_pop: unit -> 'a pop_result;  (** Try to pop an item right now. *)
  close: unit -> unit;
      (** Close the queue. Items currently in the queue will still be accessible
          to consumers until the queue is emptied out. Idempotent. *)
  closed: unit -> bool;
      (** Is the queue closed {b for writing}. Consumers should only use
          [try_pop] because a queue that's closed-for-writing might still
          contain straggler items that need to be consumed.

          This should be as fast and cheap as possible. *)
}
(** A bounded queue, with multiple producers and potentially multiple consumers.

    All functions must be thread-safe except for [try_pop] which might not have
    to be depending on the context (e.g. a Lwt-specific queue implementation
    will consume only from the Lwt thread). *)

let[@inline] push (self : _ t) x : unit = self.push x

let[@inline] num_discarded self = self.num_discarded ()

let[@inline] try_pop (self : _ t) : _ pop_result = self.try_pop ()

let[@inline] on_non_empty (self : _ t) f = self.on_non_empty f

let[@inline] close (self : _ t) : unit = self.close ()

let[@inline] closed (self : _ t) : bool = self.closed ()

(** Turn the writing end of the queue into an emitter.
    @param close_queue_on_close
      if true, closing the emitter will close the queue *)
let to_emitter ~close_queue_on_close (self : 'a t) :
    'a Opentelemetry_emitter.Emitter.t =
  let closed () = self.closed () in
  let enabled () = not (closed ()) in
  let emit x = if x <> [] then push self x in
  let tick ~now:_ = () in

  (* NOTE: we cannot actually flush, only close. Emptying the queue is
     fundamentally asynchronous because it's done by consumers *)
  let flush_and_close () = if close_queue_on_close then close self in
  { closed; enabled; emit; tick; flush_and_close }

module Defaults = struct
  (** The default high watermark *)
  let high_watermark : int = 2048
end
