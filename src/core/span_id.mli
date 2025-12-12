(** Unique ID of a span. *)

type t

val create : unit -> t

val dummy : t

val pp : Format.formatter -> t -> unit

val is_valid : t -> bool

val to_bytes : t -> bytes

val of_bytes : bytes -> t

val to_hex : t -> string

val to_hex_into : t -> bytes -> int -> unit

val of_hex : string -> t

val of_hex_substring : string -> int -> t
