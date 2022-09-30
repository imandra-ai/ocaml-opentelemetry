(** Atomic list *)

type 'a t

val make : unit -> 'a t

val add : 'a t -> 'a -> unit

val pop_all : 'a t -> 'a list
