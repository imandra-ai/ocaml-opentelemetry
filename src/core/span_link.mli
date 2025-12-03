(** Span Link

    A pointer from the current span to another span in the same trace or in a
    different trace. For example, this can be used in batching operations, where
    a single batch handler processes multiple requests from different traces or
    when the handler receives a request from a different project. *)

open Common_
open Proto.Trace

type t = span_link

val make :
  trace_id:Trace_id.t ->
  span_id:Span_id.t ->
  ?trace_state:string ->
  ?attrs:Key_value.t list ->
  ?dropped_attributes_count:int ->
  unit ->
  t

val of_span_ctx :
  ?trace_state:string ->
  ?attrs:Key_value.t list ->
  ?dropped_attributes_count:int ->
  Span_ctx.t ->
  t
