(** A collection of callbacks. thread-safe. *)

type t
(** Thread safe set of callbacks *)

val create : unit -> t

val register : t -> (unit -> unit) -> unit

val trigger : t -> unit

val clear : t -> unit
(** Remove all callbacks. *)
