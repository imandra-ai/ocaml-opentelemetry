(** Traces.

    See
    {{:https://opentelemetry.io/docs/reference/specification/overview/#tracing-signal}
     the spec} *)

open Common_
open Proto.Trace
open Opentelemetry_emitter

type span = Span.t

type t = Span.t Emitter.t
(** A tracer.

    https://opentelemetry.io/docs/specs/otel/trace/api/#tracer *)

(** Dummy tracer, always disabled *)
let dummy () : t = Emitter.dummy ()

(** A tracer that uses the current {!Main_exporter} *)
let dynamic_forward_to_main_exporter : t =
  Main_exporter.Util.dynamic_forward_to_main_exporter () ~get_emitter:(fun e ->
      e.emit_spans)
