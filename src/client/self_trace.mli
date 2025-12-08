(** Mini tracing module (disabled if [config.self_trace=false]) *)

open Common_

val add_event : OTEL.Span.t -> OTEL.Event.t -> unit

val with_ :
  ?kind:OTEL.Span_kind.t ->
  ?attrs:(string * OTEL.value) list ->
  string ->
  (OTEL.Span.t -> 'a) ->
  'a
(** A simple way to create spans to instrument parts of the OTEL SDK itself. *)

val set_tracer : OTEL.Tracer.t -> unit
(** Set the tracer to use for self-tracing. We need to make sure it will not
    lead to infinite loops (if the tracer itself is self-tracing, it might
    invoke itself recursively, and so on). *)

val set_enabled : bool -> unit
(** Enable self tracing. A tracer must also be set. *)
