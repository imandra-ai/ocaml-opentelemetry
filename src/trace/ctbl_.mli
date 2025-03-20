(** Concurrent hashmap *)

type 'a t

val create : unit -> 'a t

val find : 'a t -> int64 -> 'a option

val add : 'a t -> int64 -> 'a -> unit

val remove : 'a t -> int64 -> unit
