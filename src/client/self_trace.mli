(** Mini tracing module (disabled if [config.self_trace=false]) *)

val add_event : Opentelemetry.Scope.t -> Opentelemetry.Event.t -> unit

val with_ :
  ?kind:Opentelemetry.Span_kind.t ->
  ?attrs:(string * Opentelemetry.value) list ->
  string ->
  (Opentelemetry.Scope.t -> 'a) ->
  'a

val set_enabled : bool -> unit
