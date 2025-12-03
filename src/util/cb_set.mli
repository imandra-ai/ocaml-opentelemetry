(** A collection of callbacks. thread-safe. *)

type t

val create : unit -> t

val register : t -> (unit -> unit) -> unit

val trigger : t -> unit
