(** A collection of callbacks that are regularly called. *)

type t

val create : unit -> t

val on_tick : t -> (unit -> unit) -> unit

val tick : t -> unit
