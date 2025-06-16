(** Mini tracing module (disabled if [config.self_trace=false]) *)

val add_event :
  Opentelemetry.Scope.t -> Opentelemetry_proto.Trace.span_event -> unit

val with_ :
  ?kind:Opentelemetry_proto.Trace.span_span_kind ->
  ?attrs:(string * Opentelemetry.value) list ->
  string ->
  (Opentelemetry.Scope.t -> 'a) ->
  'a

val set_enabled : bool -> unit
