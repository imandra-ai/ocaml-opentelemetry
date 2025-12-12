(** Main Opentelemetry API for libraries and user code. *)

module Core = Opentelemetry_core
(** Core types and definitions *)

module Interval_limiter = Interval_limiter
(** Utility to limit the frequency of some event
    @since NEXT_RELEASE *)

(** {2 Wire format} *)

module Proto = Opentelemetry_proto
(** Protobuf types.

    This is mostly useful internally. Users should not need to touch it. *)

(** {2 Timestamps} *)

module Timestamp_ns = Timestamp_ns

(** {2 Export signals to some external collector.} *)

module Emitter = Opentelemetry_emitter.Emitter
module Exporter = Exporter
module Main_exporter = Main_exporter

module Collector = struct
  include Exporter
  include Main_exporter
end
[@@deprecated "Use 'Exporter' instead"]

(** {2 Identifiers} *)

module Trace_id = Trace_id

let k_trace_id = Trace_id.k_trace_id

module Span_id = Span_id
module Span_ctx = Span_ctx

let k_span_ctx = Span_ctx.k_span_ctx

(** {2 Attributes and conventions} *)

module Conventions = Conventions

type value = Value.t
(** A value in a key/value attribute *)

type key_value = Key_value.t

(** {2 Global settings} *)

module Globals = Globals

(** {2 Traces and Spans} *)

module Event = Event
module Span_link = Span_link
module Span_status = Span_status
module Span_kind = Span_kind

(** {2 Traces} *)

module Span = Span
module Ambient_span = Ambient_span
module Tracer = Tracer
module Trace = Tracer [@@deprecated "use Tracer instead"]

(** {2 Metrics} *)

module Metrics = Metrics
module Metrics_callbacks = Metrics_callbacks
module Metrics_emitter = Metrics_emitter

(** {2 Logs} *)

module Log_record = Log_record
module Logger = Logger
module Logs = Logger [@@deprecated "use Logger"]

(** {2 Utils} *)

module Any_signal = Any_signal
module Any_signal_l = Any_signal_l
module Trace_context = Trace_context
module Gc_metrics = Gc_metrics

module Aswitch = Aswitch
(** @since NEXT_RELEASE *)

module Alist = Alist
(** Atomic list, for internal usage
    @since 0.7 *)

(* *)

module GC_metrics = Gc_metrics
[@@deprecated "use Gc_metrics (beware capitalization)"]
