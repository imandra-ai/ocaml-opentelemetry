(** Span kind.
    @since 0.11 *)

open Common_
open Proto.Trace

type t = span_span_kind =
  | Span_kind_unspecified
  | Span_kind_internal
  | Span_kind_server
  | Span_kind_client
  | Span_kind_producer
  | Span_kind_consumer
