type t

val create : min_interval:Mtime.span -> unit -> t

val make_attempt : t -> bool
(** [make_attempt lim] returns [true] if the last successful attempt was more
    than [min_interval] ago, as measured by mtime. If so, this counts as the new
    latest attempt; otherwise [false] is returned and the state is not updated.
*)
