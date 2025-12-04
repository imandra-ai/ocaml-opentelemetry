(** Mini tracing module (disabled if [config.self_trace=false]) *)

open Common_

val add_event : OTEL.Span.t -> OTEL.Event.t -> unit

val with_ :
  ?kind:OTEL.Span_kind.t ->
  ?attrs:(string * OTEL.value) list ->
  string ->
  (OTEL.Span.t -> 'a) ->
  'a

val set_enabled : bool -> unit
