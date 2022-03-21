
open Opentelemetry

module Span_id = Span_id
module Trace_id = Trace_id
module Span = Span
module Globals = Globals
module Timestamp_ns = Timestamp_ns

module Trace = struct
  open Proto.Trace
  include Trace

  (** Emit asynchronously *)
  let emit (spans:span list) : unit Lwt.t =
    let fut, wake = Lwt.wait() in
    let ils =
      default_instrumentation_library_spans ~spans () in
    let rs = default_resource_spans ~instrumentation_library_spans:[ils] () in
    Collector.send_trace [rs]
      ~over:(fun () -> Lwt.wakeup_later wake ())
      ~ret:(fun () -> fut)
end

module Metrics = struct
  open Proto.Metrics
  include Metrics

  (** Emit some metrics to the collector. *)
  let emit (l:t list) : unit Lwt.t =
    let fut, wake = Lwt.wait() in
    let lm =
      default_instrumentation_library_metrics ~metrics:l () in
    let rm = default_resource_metrics
        ~instrumentation_library_metrics:[lm] () in
    Collector.send_metrics [rm]
      ~over:(fun () -> Lwt.wakeup_later wake ())
      ~ret:(fun () -> fut)
end
