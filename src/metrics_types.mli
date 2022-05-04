(** metrics.proto Types *)



(** {2 Types} *)

type exemplar_value =
  | As_double of float
  | As_int of int64

and exemplar = {
  filtered_attributes : Common_types.key_value list;
  time_unix_nano : int64;
  value : exemplar_value;
  span_id : bytes;
  trace_id : bytes;
}

type number_data_point_value =
  | As_double of float
  | As_int of int64

and number_data_point = {
  attributes : Common_types.key_value list;
  start_time_unix_nano : int64;
  time_unix_nano : int64;
  value : number_data_point_value;
  exemplars : exemplar list;
  flags : int32;
}

type gauge = {
  data_points : number_data_point list;
}

type aggregation_temporality =
  | Aggregation_temporality_unspecified 
  | Aggregation_temporality_delta 
  | Aggregation_temporality_cumulative 

type sum = {
  data_points : number_data_point list;
  aggregation_temporality : aggregation_temporality;
  is_monotonic : bool;
}

type histogram_data_point = {
  attributes : Common_types.key_value list;
  start_time_unix_nano : int64;
  time_unix_nano : int64;
  count : int64;
  sum : float;
  bucket_counts : int64 list;
  explicit_bounds : float list;
  exemplars : exemplar list;
  flags : int32;
}

type histogram = {
  data_points : histogram_data_point list;
  aggregation_temporality : aggregation_temporality;
}

type exponential_histogram_data_point_buckets = {
  offset : int32;
  bucket_counts : int64 list;
}

type exponential_histogram_data_point = {
  attributes : Common_types.key_value list;
  start_time_unix_nano : int64;
  time_unix_nano : int64;
  count : int64;
  sum : float;
  scale : int32;
  zero_count : int64;
  positive : exponential_histogram_data_point_buckets option;
  negative : exponential_histogram_data_point_buckets option;
  flags : int32;
  exemplars : exemplar list;
}

type exponential_histogram = {
  data_points : exponential_histogram_data_point list;
  aggregation_temporality : aggregation_temporality;
}

type summary_data_point_value_at_quantile = {
  quantile : float;
  value : float;
}

type summary_data_point = {
  attributes : Common_types.key_value list;
  start_time_unix_nano : int64;
  time_unix_nano : int64;
  count : int64;
  sum : float;
  quantile_values : summary_data_point_value_at_quantile list;
  flags : int32;
}

type summary = {
  data_points : summary_data_point list;
}

type metric_data =
  | Gauge of gauge
  | Sum of sum
  | Histogram of histogram
  | Exponential_histogram of exponential_histogram
  | Summary of summary

and metric = {
  name : string;
  description : string;
  unit_ : string;
  data : metric_data;
}

type instrumentation_library_metrics = {
  instrumentation_library : Common_types.instrumentation_library option;
  metrics : metric list;
  schema_url : string;
}

type resource_metrics = {
  resource : Resource_types.resource option;
  instrumentation_library_metrics : instrumentation_library_metrics list;
  schema_url : string;
}

type metrics_data = {
  resource_metrics : resource_metrics list;
}

type data_point_flags =
  | Flag_none 
  | Flag_no_recorded_value 


(** {2 Default values} *)

val default_exemplar_value : unit -> exemplar_value
(** [default_exemplar_value ()] is the default value for type [exemplar_value] *)

val default_exemplar : 
  ?filtered_attributes:Common_types.key_value list ->
  ?time_unix_nano:int64 ->
  ?value:exemplar_value ->
  ?span_id:bytes ->
  ?trace_id:bytes ->
  unit ->
  exemplar
(** [default_exemplar ()] is the default value for type [exemplar] *)

val default_number_data_point_value : unit -> number_data_point_value
(** [default_number_data_point_value ()] is the default value for type [number_data_point_value] *)

val default_number_data_point : 
  ?attributes:Common_types.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?value:number_data_point_value ->
  ?exemplars:exemplar list ->
  ?flags:int32 ->
  unit ->
  number_data_point
(** [default_number_data_point ()] is the default value for type [number_data_point] *)

val default_gauge : 
  ?data_points:number_data_point list ->
  unit ->
  gauge
(** [default_gauge ()] is the default value for type [gauge] *)

val default_aggregation_temporality : unit -> aggregation_temporality
(** [default_aggregation_temporality ()] is the default value for type [aggregation_temporality] *)

val default_sum : 
  ?data_points:number_data_point list ->
  ?aggregation_temporality:aggregation_temporality ->
  ?is_monotonic:bool ->
  unit ->
  sum
(** [default_sum ()] is the default value for type [sum] *)

val default_histogram_data_point : 
  ?attributes:Common_types.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?count:int64 ->
  ?sum:float ->
  ?bucket_counts:int64 list ->
  ?explicit_bounds:float list ->
  ?exemplars:exemplar list ->
  ?flags:int32 ->
  unit ->
  histogram_data_point
(** [default_histogram_data_point ()] is the default value for type [histogram_data_point] *)

val default_histogram : 
  ?data_points:histogram_data_point list ->
  ?aggregation_temporality:aggregation_temporality ->
  unit ->
  histogram
(** [default_histogram ()] is the default value for type [histogram] *)

val default_exponential_histogram_data_point_buckets : 
  ?offset:int32 ->
  ?bucket_counts:int64 list ->
  unit ->
  exponential_histogram_data_point_buckets
(** [default_exponential_histogram_data_point_buckets ()] is the default value for type [exponential_histogram_data_point_buckets] *)

val default_exponential_histogram_data_point : 
  ?attributes:Common_types.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?count:int64 ->
  ?sum:float ->
  ?scale:int32 ->
  ?zero_count:int64 ->
  ?positive:exponential_histogram_data_point_buckets option ->
  ?negative:exponential_histogram_data_point_buckets option ->
  ?flags:int32 ->
  ?exemplars:exemplar list ->
  unit ->
  exponential_histogram_data_point
(** [default_exponential_histogram_data_point ()] is the default value for type [exponential_histogram_data_point] *)

val default_exponential_histogram : 
  ?data_points:exponential_histogram_data_point list ->
  ?aggregation_temporality:aggregation_temporality ->
  unit ->
  exponential_histogram
(** [default_exponential_histogram ()] is the default value for type [exponential_histogram] *)

val default_summary_data_point_value_at_quantile : 
  ?quantile:float ->
  ?value:float ->
  unit ->
  summary_data_point_value_at_quantile
(** [default_summary_data_point_value_at_quantile ()] is the default value for type [summary_data_point_value_at_quantile] *)

val default_summary_data_point : 
  ?attributes:Common_types.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?count:int64 ->
  ?sum:float ->
  ?quantile_values:summary_data_point_value_at_quantile list ->
  ?flags:int32 ->
  unit ->
  summary_data_point
(** [default_summary_data_point ()] is the default value for type [summary_data_point] *)

val default_summary : 
  ?data_points:summary_data_point list ->
  unit ->
  summary
(** [default_summary ()] is the default value for type [summary] *)

val default_metric_data : unit -> metric_data
(** [default_metric_data ()] is the default value for type [metric_data] *)

val default_metric : 
  ?name:string ->
  ?description:string ->
  ?unit_:string ->
  ?data:metric_data ->
  unit ->
  metric
(** [default_metric ()] is the default value for type [metric] *)

val default_instrumentation_library_metrics : 
  ?instrumentation_library:Common_types.instrumentation_library option ->
  ?metrics:metric list ->
  ?schema_url:string ->
  unit ->
  instrumentation_library_metrics
(** [default_instrumentation_library_metrics ()] is the default value for type [instrumentation_library_metrics] *)

val default_resource_metrics : 
  ?resource:Resource_types.resource option ->
  ?instrumentation_library_metrics:instrumentation_library_metrics list ->
  ?schema_url:string ->
  unit ->
  resource_metrics
(** [default_resource_metrics ()] is the default value for type [resource_metrics] *)

val default_metrics_data : 
  ?resource_metrics:resource_metrics list ->
  unit ->
  metrics_data
(** [default_metrics_data ()] is the default value for type [metrics_data] *)

val default_data_point_flags : unit -> data_point_flags
(** [default_data_point_flags ()] is the default value for type [data_point_flags] *)
