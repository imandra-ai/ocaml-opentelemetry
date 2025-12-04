(** Storing the current span in ambient context. *)

val get : unit -> Span.t option
(** Find current span from ambient-context *)

val with_ambient : Span.t -> (unit -> 'a) -> 'a
(** [with_ambient span f] runs [f()] with the current ambient span being set to
    [span] *)
