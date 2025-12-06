(** Backoff behavior in case of errors *)

type t
(** Backoff state. Not thread safe *)

val create : unit -> t

val on_success : t -> unit

val on_error : t -> unit

val cur_duration_s : t -> float
