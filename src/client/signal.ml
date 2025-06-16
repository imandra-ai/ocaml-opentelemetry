module Trace_service = Opentelemetry.Proto.Trace_service
module Metrics_service = Opentelemetry.Proto.Metrics_service
module Logs_service = Opentelemetry.Proto.Logs_service
module Span = Opentelemetry.Span

let ( let@ ) f x = f x

module Converter () = struct
  let encoder = Pbrt.Encoder.create ()

  let resource_to_string ~ctor ~enc resource =
    let x = ctor resource in
    let@ _sc = Self_trace.with_ ~kind:Span.Span_kind_internal "encode-proto" in
    Pbrt.Encoder.reset encoder;
    enc x encoder;
    Pbrt.Encoder.to_string encoder

  let logs resource_logs =
    resource_logs
    |> resource_to_string
         ~ctor:(fun r ->
           Logs_service.default_export_logs_service_request ~resource_logs:r ())
         ~enc:Logs_service.encode_pb_export_logs_service_request

  let metrics resource_metrics =
    resource_metrics
    |> resource_to_string
         ~ctor:(fun r ->
           Metrics_service.default_export_metrics_service_request
             ~resource_metrics:r ())
         ~enc:Metrics_service.encode_pb_export_metrics_service_request

  let traces resource_spans =
    resource_spans
    |> resource_to_string
         ~ctor:(fun r ->
           Trace_service.default_export_trace_service_request ~resource_spans:r
             ())
         ~enc:Trace_service.encode_pb_export_trace_service_request
end
