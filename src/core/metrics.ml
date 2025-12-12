(** Metrics.

    See
    {{:https://opentelemetry.io/docs/reference/specification/overview/#metric-signal}
     the spec} *)

open Common_
open Proto
open Proto.Metrics

type t = Metrics.metric
(** A single metric, measuring some time-varying quantity or statistical
    distribution. It is composed of one or more data points that have precise
    values and time stamps. Each distinct metric should have a distinct name. *)

let pp = Proto.Metrics.pp_metric

open struct
  let _program_start = Timestamp_ns.now_unix_ns ()
end

(** Number data point, as a float *)
let float ?start_time_unix_nano ?(now = Timestamp_ns.now_unix_ns ())
    ?(attrs = []) (d : float) : number_data_point =
  let attributes = attrs |> List.map Key_value.conv in
  make_number_data_point ?start_time_unix_nano ~time_unix_nano:now ~attributes
    ~value:(As_double d) ()

(** Number data point, as an int *)
let int ?start_time_unix_nano ?(now = Timestamp_ns.now_unix_ns ()) ?(attrs = [])
    (i : int) : number_data_point =
  let attributes = attrs |> List.map Key_value.conv in
  make_number_data_point ?start_time_unix_nano ~time_unix_nano:now ~attributes
    ~value:(As_int (Int64.of_int i))
    ()

(** Aggregation of a scalar metric, always with the current value *)
let gauge ~name ?description ?unit_ (l : number_data_point list) : t =
  let data = Gauge (make_gauge ~data_points:l ()) in
  make_metric ~name ?description ?unit_ ~data ()

type aggregation_temporality = Metrics.aggregation_temporality =
  | Aggregation_temporality_unspecified
  | Aggregation_temporality_delta
  | Aggregation_temporality_cumulative

(** Sum of all reported measurements over a time interval *)
let sum ~name ?description ?unit_
    ?(aggregation_temporality = Aggregation_temporality_cumulative)
    ?is_monotonic (l : number_data_point list) : t =
  let data =
    Sum (make_sum ~data_points:l ?is_monotonic ~aggregation_temporality ())
  in
  make_metric ~name ?description ?unit_ ~data ()

(** Histogram data
    @param count number of values in population (non negative)
    @param sum sum of values in population (0 if count is 0)
    @param bucket_counts
      count value of histogram for each bucket. Sum of the counts must be equal
      to [count]. length must be [1+length explicit_bounds]
    @param explicit_bounds strictly increasing list of bounds for the buckets *)
let histogram_data_point ?start_time_unix_nano
    ?(now = Timestamp_ns.now_unix_ns ()) ?(attrs = []) ?(exemplars = [])
    ?(explicit_bounds = []) ?sum ~bucket_counts ~count () : histogram_data_point
    =
  let attributes = attrs |> List.map Key_value.conv in
  make_histogram_data_point ?start_time_unix_nano ~time_unix_nano:now
    ~attributes ~exemplars ~bucket_counts ~explicit_bounds ~count ?sum ()

let histogram ~name ?description ?unit_ ?aggregation_temporality
    (l : histogram_data_point list) : t =
  let data =
    Histogram (make_histogram ~data_points:l ?aggregation_temporality ())
  in
  make_metric ~name ?description ?unit_ ~data ()

(* TODO: exponential history *)
(* TODO: summary *)
(* TODO: exemplar *)
