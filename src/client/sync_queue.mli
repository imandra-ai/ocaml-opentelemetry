(** Simple blocking queue *)

type 'a t

val create : unit -> _ t

exception Closed

val push : 'a t -> 'a -> unit
(** [push q x] pushes [x] into [q], and returns [()].
    @raise Closed if [close q] was previously called.*)

val pop : 'a t -> 'a
(** [pop q] pops the next element in [q]. It might block until an element comes.
    @raise Closed if the queue was closed before a new element was available. *)

val try_pop : 'a t -> 'a option

val pop_all : 'a t -> 'a Queue.t -> unit
(** [pop_all q into] pops all the elements of [q] and moves them into [into]. if
    no element is available, it will block until it successfully transfers at
    least one item to [into].
    @raise Closed if the queue was closed before a new element was available. *)

val closed : _ t -> bool

val close : _ t -> unit
(** Close the queue, meaning there won't be any more [push] allowed. *)

val push_while_not_full : high_watermark:int -> 'a t -> 'a list -> int * int
(** [push_while_not_full q ~high_watermark xs] tries to push each item of [x]
    into [q].

    An item is not pushed if the queue is "full" (size >= high_watermark).

    This returns a pair [num_discarded, old_size] where [num_discarded] is the
    number of items that could not be pushed, and [old_size] is the size before
    anything was pushed. *)
