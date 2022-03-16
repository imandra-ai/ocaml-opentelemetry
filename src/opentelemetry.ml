
(** Protobuf types *)
module Proto = struct
  module Common = struct
    include Common_types
    include Common_pp
    include Common_pb
  end

  module Resource = struct
    include Resource_types
    include Resource_pp
    include Resource_pb
  end

  module Trace = struct
    include Trace_types
    include Trace_pp
    include Trace_pb
  end

  module Metrics = struct
    include Metrics_types
    include Metrics_pp
    include Metrics_pb
  end

  module Trace_service = struct
    include Trace_service_types
    include Trace_service_pb
    include Trace_service_pp
  end

  module Metrics_service = struct
    include Metrics_service_types
    include Metrics_service_pp
    include Metrics_service_pb
  end

  module Status = struct
    include Status_types
    include Status_pp
    include Status_pb
  end
end

(** Utils *)
module Util = struct
  let ns_in_a_day = Int64.(mul 1_000_000_000L (of_int (24 * 3600)))

  (** Current unix timestamp in nanoseconds *)
  let[@inline] now_unix_ns () =
    let span = Ptime_clock.now() |> Ptime.to_span in
    let d, ps = Ptime.Span.to_d_ps span in
    let d = Int64.(mul (of_int d) ns_in_a_day) in
    let ns = Int64.(div ps 1_000L) in
    Int64.(add d ns)
end

(** Collector types

    These types are used by backend implementations, to send events to
    collectors such as Jaeger.

    Note: most users will not need to touch this module *)
module Collector = struct
  open Proto

  (** Collector client interface. *)
  module type BACKEND = sig
    val send_trace : Trace_service.export_trace_service_request -> unit

    val send_metrics : Metrics_service.export_metrics_service_request -> unit

    val cleanup : unit -> unit
  end

  type backend = (module BACKEND)

  let backend : backend option ref = ref None

  let send_trace (l:Trace.resource_spans list) : unit =
    match !backend with
    | None -> ()
    | Some (module B) ->
      let ev = Trace_service.default_export_trace_service_request
          ~resource_spans:l () in
      B.send_trace ev

  let send_metrics (l:Metrics.resource_metrics list) : unit =
    match !backend with
    | None -> ()
    | Some (module B) ->
      let ev = Metrics_service.default_export_metrics_service_request
          ~resource_metrics:l () in
      B.send_metrics ev
end

(** Traces.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#tracing-signal} the spec} *)
module Trace = struct
  open Proto.Trace
end

(** Metrics.

    See {{: https://opentelemetry.io/docs/reference/specification/overview/#metric-signal} the spec} *)
module Metrics = struct
  open Metrics_types

  type t = Metrics_types.metric

  (** Number data point, as a float *)
  let float ?start_time_unix_nano
      ?(now=Util.now_unix_ns())
      (d:float) : number_data_point =
    default_number_data_point ?start_time_unix_nano ~time_unix_nano:now
      ~value:(As_double d) ()

  (** Number data point, as an int *)
  let int ?start_time_unix_nano
      ?(now=Util.now_unix_ns())
      (i:int) : number_data_point =
    default_number_data_point ?start_time_unix_nano ~time_unix_nano:now
      ~value:(As_int (Int64.of_int i)) ()

  (** Aggregation of a scalar metric, always with the current value *)
  let gauge ~name ?description ?unit_ (l:number_data_point list) : t =
    let data = Gauge (default_gauge ~data_points:l ()) in
    default_metric ~name ?description ?unit_ ~data ()

  type aggregation_temporality = Metrics_types.aggregation_temporality =
    | Aggregation_temporality_unspecified
    | Aggregation_temporality_delta
    | Aggregation_temporality_cumulative

  (** Sum of all reported measurements over a time interval *)
  let sum ~name ?description ?unit_
      ?aggregation_temporality ?is_monotonic
      (l:number_data_point list) : t =
    let data =
      Sum (default_sum ~data_points:l ?is_monotonic
             ?aggregation_temporality ()) in
    default_metric ~name ?description ?unit_ ~data ()

  (* TODO
  let histogram ~name ?description ?unit_
      ?aggregation_temporality
      (l:number_data_point list) : t =
    let data h=
      Histogram (default_histogram ~data_points:l
             ?aggregation_temporality ()) in
    default_metric ~name ?description ?unit_ ~data ()
     *)

  (* TODO: exponential history *)
  (* TODO: summary *)
  (* TODO: exemplar *)

  (** Emit a bunch of metrics to the collector. *)
  let emit (l:t list) : unit =
    let lm =
      default_instrumentation_library_metrics ~metrics:l () in
    let rm = default_resource_metrics
        ~instrumentation_library_metrics:[lm] () in
    Collector.send_metrics [rm]
end

(*
module Span = Span
module Timestamp = Timestamp
   *)

