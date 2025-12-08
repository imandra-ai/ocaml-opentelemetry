(** Generate random identifiers.

    We need random identifiers for trace IDs and span IDs. *)

val rand_bytes_16_ref : (unit -> bytes) ref
(** Generate 16 bytes of random data. The implementation can be swapped to use
    any random generator. *)

val rand_bytes_8_ref : (unit -> bytes) ref
(** Generate 8 bytes of random data. The implementation can be swapped to use
    any random generator. *)

val default_rand_bytes_8 : unit -> bytes
(** Default implementation using {!Random} *)

val default_rand_bytes_16 : unit -> bytes
(** Default implementation using {!Random} *)

val rand_bytes_16 : unit -> bytes
(** Call the current {!rand_bytes_16_ref} *)

val rand_bytes_8 : unit -> bytes
(** Call the current {!rand_bytes_8_ref} *)
