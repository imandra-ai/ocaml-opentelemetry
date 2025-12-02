(** Atomic list.

    Note that we add at the front, so [add x 1; add x 2; pop_all x] will return
    [2;1]. *)

type 'a t

val get : 'a t -> 'a list
(** Snapshot *)

val is_empty : _ t -> bool

val make : unit -> 'a t

val add : 'a t -> 'a -> unit

val pop_all : 'a t -> 'a list
