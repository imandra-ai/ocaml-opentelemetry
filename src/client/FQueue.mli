(** queue of fixed size *)

type 'a t

val create : dummy:'a -> int -> 'a t

val size : _ t -> int

val push : 'a t -> 'a -> bool (* true iff it could write element *)

val pop_iter_all : 'a t -> ('a -> unit) -> unit
