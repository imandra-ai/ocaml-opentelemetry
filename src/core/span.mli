(** Spans.

    A Span is the workhorse of traces, it indicates an operation that took place
    over a given span of time (indicated by start_time and end_time) as part of
    a hierarchical trace. All spans in a given trace are bound by the use of the
    same {!Trace_id.t}. *)

open Common_
open Proto.Trace

type t = span

type id = Span_id.t

type kind = Span_kind.t =
  | Span_kind_unspecified
  | Span_kind_internal
  | Span_kind_server
  | Span_kind_client
  | Span_kind_producer
  | Span_kind_consumer

val id : t -> Span_id.t

type key_value = Key_value.t

val create :
  ?kind:kind ->
  ?id:id ->
  ?trace_state:string ->
  ?attrs:key_value list ->
  ?events:Event.t list ->
  ?status:status ->
  trace_id:Trace_id.t ->
  ?parent:id ->
  ?links:Span_link.t list ->
  start_time:Timestamp_ns.t ->
  end_time:Timestamp_ns.t ->
  string ->
  t * id
(** [create ~trace_id name] creates a new span with its unique ID.
    @param trace_id the trace this belongs to
    @param parent parent span, if any
    @param links
      list of links to other spans, each with their trace state (see
      {{:https://www.w3.org/TR/trace-context/#tracestate-header} w3.org}) *)
