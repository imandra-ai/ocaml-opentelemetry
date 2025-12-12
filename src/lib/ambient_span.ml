(** Find current span from ambient-context *)
let[@inline] get () : Span.t option =
  Opentelemetry_ambient_context.get Span.k_context

(** [with_ambient span f] runs [f()] with the current ambient span being set to
    [span] *)
let[@inline] with_ambient (span : Span.t) (f : unit -> 'a) : 'a =
  Opentelemetry_ambient_context.with_key_bound_to Span.k_context span (fun _ ->
      f ())
