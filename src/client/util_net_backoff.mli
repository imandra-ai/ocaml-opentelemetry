(** Backoff behavior in case of errors *)

type t
(** Backoff state for networking operations. Not thread safe. Do remember to add
    a bit of jitter. *)

val create : unit -> t

val on_success : t -> unit
(** Reset backoff to its baseline. *)

val on_error : t -> float
(** Increase backoff, returning the current delay in seconds *)
