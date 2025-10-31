
(** Code for metrics.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/metrics/v1/metrics.proto", do not edit *)



(** {2 Types} *)

type exemplar_value =
  | As_double of float
  | As_int of int64

and exemplar = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 3 fields *)
  mutable filtered_attributes : Common.key_value list;
  mutable time_unix_nano : int64;
  mutable value : exemplar_value option;
  mutable span_id : bytes;
  mutable trace_id : bytes;
}

type number_data_point_value =
  | As_double of float
  | As_int of int64

and number_data_point = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 3 fields *)
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable value : number_data_point_value option;
  mutable exemplars : exemplar list;
  mutable flags : int32;
}

type gauge = private {
  mutable data_points : number_data_point list;
}

type aggregation_temporality =
  | Aggregation_temporality_unspecified 
  | Aggregation_temporality_delta 
  | Aggregation_temporality_cumulative 

type sum = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 2 fields *)
  mutable data_points : number_data_point list;
  mutable aggregation_temporality : aggregation_temporality;
  mutable is_monotonic : bool;
}

type histogram_data_point = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 4 fields *)
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float option;
  mutable bucket_counts : int64 list;
  mutable explicit_bounds : float list;
  mutable exemplars : exemplar list;
  mutable flags : int32;
  mutable min : float option;
  mutable max : float option;
}

type histogram = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 1 fields *)
  mutable data_points : histogram_data_point list;
  mutable aggregation_temporality : aggregation_temporality;
}

type exponential_histogram_data_point_buckets = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 1 fields *)
  mutable offset : int32;
  mutable bucket_counts : int64 list;
}

type exponential_histogram_data_point = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 7 fields *)
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float option;
  mutable scale : int32;
  mutable zero_count : int64;
  mutable positive : exponential_histogram_data_point_buckets option;
  mutable negative : exponential_histogram_data_point_buckets option;
  mutable flags : int32;
  mutable exemplars : exemplar list;
  mutable min : float option;
  mutable max : float option;
  mutable zero_threshold : float;
}

type exponential_histogram = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 1 fields *)
  mutable data_points : exponential_histogram_data_point list;
  mutable aggregation_temporality : aggregation_temporality;
}

type summary_data_point_value_at_quantile = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 2 fields *)
  mutable quantile : float;
  mutable value : float;
}

type summary_data_point = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 5 fields *)
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float;
  mutable quantile_values : summary_data_point_value_at_quantile list;
  mutable flags : int32;
}

type summary = private {
  mutable data_points : summary_data_point list;
}

type metric_data =
  | Gauge of gauge
  | Sum of sum
  | Histogram of histogram
  | Exponential_histogram of exponential_histogram
  | Summary of summary

and metric = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 3 fields *)
  mutable name : string;
  mutable description : string;
  mutable unit_ : string;
  mutable data : metric_data option;
  mutable metadata : Common.key_value list;
}

type scope_metrics = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 1 fields *)
  mutable scope : Common.instrumentation_scope option;
  mutable metrics : metric list;
  mutable schema_url : string;
}

type resource_metrics = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 1 fields *)
  mutable resource : Resource.resource option;
  mutable scope_metrics : scope_metrics list;
  mutable schema_url : string;
}

type metrics_data = private {
  mutable resource_metrics : resource_metrics list;
}

type data_point_flags =
  | Data_point_flags_do_not_use 
  | Data_point_flags_no_recorded_value_mask 


(** {2 Basic values} *)

val default_exemplar_value : unit -> exemplar_value
(** [default_exemplar_value ()] is a new empty value for type [exemplar_value] *)

val default_exemplar : unit -> exemplar 
(** [default_exemplar ()] is a new empty value for type [exemplar] *)

val default_number_data_point_value : unit -> number_data_point_value
(** [default_number_data_point_value ()] is a new empty value for type [number_data_point_value] *)

val default_number_data_point : unit -> number_data_point 
(** [default_number_data_point ()] is a new empty value for type [number_data_point] *)

val default_gauge : unit -> gauge 
(** [default_gauge ()] is a new empty value for type [gauge] *)

val default_aggregation_temporality : unit -> aggregation_temporality
(** [default_aggregation_temporality ()] is a new empty value for type [aggregation_temporality] *)

val default_sum : unit -> sum 
(** [default_sum ()] is a new empty value for type [sum] *)

val default_histogram_data_point : unit -> histogram_data_point 
(** [default_histogram_data_point ()] is a new empty value for type [histogram_data_point] *)

val default_histogram : unit -> histogram 
(** [default_histogram ()] is a new empty value for type [histogram] *)

val default_exponential_histogram_data_point_buckets : unit -> exponential_histogram_data_point_buckets 
(** [default_exponential_histogram_data_point_buckets ()] is a new empty value for type [exponential_histogram_data_point_buckets] *)

val default_exponential_histogram_data_point : unit -> exponential_histogram_data_point 
(** [default_exponential_histogram_data_point ()] is a new empty value for type [exponential_histogram_data_point] *)

val default_exponential_histogram : unit -> exponential_histogram 
(** [default_exponential_histogram ()] is a new empty value for type [exponential_histogram] *)

val default_summary_data_point_value_at_quantile : unit -> summary_data_point_value_at_quantile 
(** [default_summary_data_point_value_at_quantile ()] is a new empty value for type [summary_data_point_value_at_quantile] *)

val default_summary_data_point : unit -> summary_data_point 
(** [default_summary_data_point ()] is a new empty value for type [summary_data_point] *)

val default_summary : unit -> summary 
(** [default_summary ()] is a new empty value for type [summary] *)

val default_metric_data : unit -> metric_data
(** [default_metric_data ()] is a new empty value for type [metric_data] *)

val default_metric : unit -> metric 
(** [default_metric ()] is a new empty value for type [metric] *)

val default_scope_metrics : unit -> scope_metrics 
(** [default_scope_metrics ()] is a new empty value for type [scope_metrics] *)

val default_resource_metrics : unit -> resource_metrics 
(** [default_resource_metrics ()] is a new empty value for type [resource_metrics] *)

val default_metrics_data : unit -> metrics_data 
(** [default_metrics_data ()] is a new empty value for type [metrics_data] *)

val default_data_point_flags : unit -> data_point_flags
(** [default_data_point_flags ()] is a new empty value for type [data_point_flags] *)


(** {2 Make functions} *)


val make_exemplar : 
  filtered_attributes:Common.key_value list ->
  ?time_unix_nano:int64 ->
  ?value:exemplar_value ->
  ?span_id:bytes ->
  ?trace_id:bytes ->
  unit ->
  exemplar
(** [make_exemplar … ()] is a builder for type [exemplar] *)

val copy_exemplar : exemplar -> exemplar

val set_exemplar_filtered_attributes : exemplar -> Common.key_value list -> unit
  (** set field filtered_attributes in exemplar *)

val has_exemplar_time_unix_nano : exemplar -> bool
  (** presence of field "time_unix_nano" in [exemplar] *)

val set_exemplar_time_unix_nano : exemplar -> int64 -> unit
  (** set field time_unix_nano in exemplar *)

val set_exemplar_value : exemplar -> exemplar_value -> unit
  (** set field value in exemplar *)

val has_exemplar_span_id : exemplar -> bool
  (** presence of field "span_id" in [exemplar] *)

val set_exemplar_span_id : exemplar -> bytes -> unit
  (** set field span_id in exemplar *)

val has_exemplar_trace_id : exemplar -> bool
  (** presence of field "trace_id" in [exemplar] *)

val set_exemplar_trace_id : exemplar -> bytes -> unit
  (** set field trace_id in exemplar *)


val make_number_data_point : 
  attributes:Common.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?value:number_data_point_value ->
  exemplars:exemplar list ->
  ?flags:int32 ->
  unit ->
  number_data_point
(** [make_number_data_point … ()] is a builder for type [number_data_point] *)

val copy_number_data_point : number_data_point -> number_data_point

val set_number_data_point_attributes : number_data_point -> Common.key_value list -> unit
  (** set field attributes in number_data_point *)

val has_number_data_point_start_time_unix_nano : number_data_point -> bool
  (** presence of field "start_time_unix_nano" in [number_data_point] *)

val set_number_data_point_start_time_unix_nano : number_data_point -> int64 -> unit
  (** set field start_time_unix_nano in number_data_point *)

val has_number_data_point_time_unix_nano : number_data_point -> bool
  (** presence of field "time_unix_nano" in [number_data_point] *)

val set_number_data_point_time_unix_nano : number_data_point -> int64 -> unit
  (** set field time_unix_nano in number_data_point *)

val set_number_data_point_value : number_data_point -> number_data_point_value -> unit
  (** set field value in number_data_point *)

val set_number_data_point_exemplars : number_data_point -> exemplar list -> unit
  (** set field exemplars in number_data_point *)

val has_number_data_point_flags : number_data_point -> bool
  (** presence of field "flags" in [number_data_point] *)

val set_number_data_point_flags : number_data_point -> int32 -> unit
  (** set field flags in number_data_point *)

val make_gauge : 
  data_points:number_data_point list ->
  unit ->
  gauge
(** [make_gauge … ()] is a builder for type [gauge] *)

val copy_gauge : gauge -> gauge

val set_gauge_data_points : gauge -> number_data_point list -> unit
  (** set field data_points in gauge *)


val make_sum : 
  data_points:number_data_point list ->
  ?aggregation_temporality:aggregation_temporality ->
  ?is_monotonic:bool ->
  unit ->
  sum
(** [make_sum … ()] is a builder for type [sum] *)

val copy_sum : sum -> sum

val set_sum_data_points : sum -> number_data_point list -> unit
  (** set field data_points in sum *)

val has_sum_aggregation_temporality : sum -> bool
  (** presence of field "aggregation_temporality" in [sum] *)

val set_sum_aggregation_temporality : sum -> aggregation_temporality -> unit
  (** set field aggregation_temporality in sum *)

val has_sum_is_monotonic : sum -> bool
  (** presence of field "is_monotonic" in [sum] *)

val set_sum_is_monotonic : sum -> bool -> unit
  (** set field is_monotonic in sum *)

val make_histogram_data_point : 
  attributes:Common.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?count:int64 ->
  ?sum:float ->
  bucket_counts:int64 list ->
  explicit_bounds:float list ->
  exemplars:exemplar list ->
  ?flags:int32 ->
  ?min:float ->
  ?max:float ->
  unit ->
  histogram_data_point
(** [make_histogram_data_point … ()] is a builder for type [histogram_data_point] *)

val copy_histogram_data_point : histogram_data_point -> histogram_data_point

val set_histogram_data_point_attributes : histogram_data_point -> Common.key_value list -> unit
  (** set field attributes in histogram_data_point *)

val has_histogram_data_point_start_time_unix_nano : histogram_data_point -> bool
  (** presence of field "start_time_unix_nano" in [histogram_data_point] *)

val set_histogram_data_point_start_time_unix_nano : histogram_data_point -> int64 -> unit
  (** set field start_time_unix_nano in histogram_data_point *)

val has_histogram_data_point_time_unix_nano : histogram_data_point -> bool
  (** presence of field "time_unix_nano" in [histogram_data_point] *)

val set_histogram_data_point_time_unix_nano : histogram_data_point -> int64 -> unit
  (** set field time_unix_nano in histogram_data_point *)

val has_histogram_data_point_count : histogram_data_point -> bool
  (** presence of field "count" in [histogram_data_point] *)

val set_histogram_data_point_count : histogram_data_point -> int64 -> unit
  (** set field count in histogram_data_point *)

val set_histogram_data_point_sum : histogram_data_point -> float -> unit
  (** set field sum in histogram_data_point *)

val set_histogram_data_point_bucket_counts : histogram_data_point -> int64 list -> unit
  (** set field bucket_counts in histogram_data_point *)

val set_histogram_data_point_explicit_bounds : histogram_data_point -> float list -> unit
  (** set field explicit_bounds in histogram_data_point *)

val set_histogram_data_point_exemplars : histogram_data_point -> exemplar list -> unit
  (** set field exemplars in histogram_data_point *)

val has_histogram_data_point_flags : histogram_data_point -> bool
  (** presence of field "flags" in [histogram_data_point] *)

val set_histogram_data_point_flags : histogram_data_point -> int32 -> unit
  (** set field flags in histogram_data_point *)

val set_histogram_data_point_min : histogram_data_point -> float -> unit
  (** set field min in histogram_data_point *)

val set_histogram_data_point_max : histogram_data_point -> float -> unit
  (** set field max in histogram_data_point *)

val make_histogram : 
  data_points:histogram_data_point list ->
  ?aggregation_temporality:aggregation_temporality ->
  unit ->
  histogram
(** [make_histogram … ()] is a builder for type [histogram] *)

val copy_histogram : histogram -> histogram

val set_histogram_data_points : histogram -> histogram_data_point list -> unit
  (** set field data_points in histogram *)

val has_histogram_aggregation_temporality : histogram -> bool
  (** presence of field "aggregation_temporality" in [histogram] *)

val set_histogram_aggregation_temporality : histogram -> aggregation_temporality -> unit
  (** set field aggregation_temporality in histogram *)

val make_exponential_histogram_data_point_buckets : 
  ?offset:int32 ->
  bucket_counts:int64 list ->
  unit ->
  exponential_histogram_data_point_buckets
(** [make_exponential_histogram_data_point_buckets … ()] is a builder for type [exponential_histogram_data_point_buckets] *)

val copy_exponential_histogram_data_point_buckets : exponential_histogram_data_point_buckets -> exponential_histogram_data_point_buckets

val has_exponential_histogram_data_point_buckets_offset : exponential_histogram_data_point_buckets -> bool
  (** presence of field "offset" in [exponential_histogram_data_point_buckets] *)

val set_exponential_histogram_data_point_buckets_offset : exponential_histogram_data_point_buckets -> int32 -> unit
  (** set field offset in exponential_histogram_data_point_buckets *)

val set_exponential_histogram_data_point_buckets_bucket_counts : exponential_histogram_data_point_buckets -> int64 list -> unit
  (** set field bucket_counts in exponential_histogram_data_point_buckets *)

val make_exponential_histogram_data_point : 
  attributes:Common.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?count:int64 ->
  ?sum:float ->
  ?scale:int32 ->
  ?zero_count:int64 ->
  ?positive:exponential_histogram_data_point_buckets ->
  ?negative:exponential_histogram_data_point_buckets ->
  ?flags:int32 ->
  exemplars:exemplar list ->
  ?min:float ->
  ?max:float ->
  ?zero_threshold:float ->
  unit ->
  exponential_histogram_data_point
(** [make_exponential_histogram_data_point … ()] is a builder for type [exponential_histogram_data_point] *)

val copy_exponential_histogram_data_point : exponential_histogram_data_point -> exponential_histogram_data_point

val set_exponential_histogram_data_point_attributes : exponential_histogram_data_point -> Common.key_value list -> unit
  (** set field attributes in exponential_histogram_data_point *)

val has_exponential_histogram_data_point_start_time_unix_nano : exponential_histogram_data_point -> bool
  (** presence of field "start_time_unix_nano" in [exponential_histogram_data_point] *)

val set_exponential_histogram_data_point_start_time_unix_nano : exponential_histogram_data_point -> int64 -> unit
  (** set field start_time_unix_nano in exponential_histogram_data_point *)

val has_exponential_histogram_data_point_time_unix_nano : exponential_histogram_data_point -> bool
  (** presence of field "time_unix_nano" in [exponential_histogram_data_point] *)

val set_exponential_histogram_data_point_time_unix_nano : exponential_histogram_data_point -> int64 -> unit
  (** set field time_unix_nano in exponential_histogram_data_point *)

val has_exponential_histogram_data_point_count : exponential_histogram_data_point -> bool
  (** presence of field "count" in [exponential_histogram_data_point] *)

val set_exponential_histogram_data_point_count : exponential_histogram_data_point -> int64 -> unit
  (** set field count in exponential_histogram_data_point *)

val set_exponential_histogram_data_point_sum : exponential_histogram_data_point -> float -> unit
  (** set field sum in exponential_histogram_data_point *)

val has_exponential_histogram_data_point_scale : exponential_histogram_data_point -> bool
  (** presence of field "scale" in [exponential_histogram_data_point] *)

val set_exponential_histogram_data_point_scale : exponential_histogram_data_point -> int32 -> unit
  (** set field scale in exponential_histogram_data_point *)

val has_exponential_histogram_data_point_zero_count : exponential_histogram_data_point -> bool
  (** presence of field "zero_count" in [exponential_histogram_data_point] *)

val set_exponential_histogram_data_point_zero_count : exponential_histogram_data_point -> int64 -> unit
  (** set field zero_count in exponential_histogram_data_point *)

val set_exponential_histogram_data_point_positive : exponential_histogram_data_point -> exponential_histogram_data_point_buckets -> unit
  (** set field positive in exponential_histogram_data_point *)

val set_exponential_histogram_data_point_negative : exponential_histogram_data_point -> exponential_histogram_data_point_buckets -> unit
  (** set field negative in exponential_histogram_data_point *)

val has_exponential_histogram_data_point_flags : exponential_histogram_data_point -> bool
  (** presence of field "flags" in [exponential_histogram_data_point] *)

val set_exponential_histogram_data_point_flags : exponential_histogram_data_point -> int32 -> unit
  (** set field flags in exponential_histogram_data_point *)

val set_exponential_histogram_data_point_exemplars : exponential_histogram_data_point -> exemplar list -> unit
  (** set field exemplars in exponential_histogram_data_point *)

val set_exponential_histogram_data_point_min : exponential_histogram_data_point -> float -> unit
  (** set field min in exponential_histogram_data_point *)

val set_exponential_histogram_data_point_max : exponential_histogram_data_point -> float -> unit
  (** set field max in exponential_histogram_data_point *)

val has_exponential_histogram_data_point_zero_threshold : exponential_histogram_data_point -> bool
  (** presence of field "zero_threshold" in [exponential_histogram_data_point] *)

val set_exponential_histogram_data_point_zero_threshold : exponential_histogram_data_point -> float -> unit
  (** set field zero_threshold in exponential_histogram_data_point *)

val make_exponential_histogram : 
  data_points:exponential_histogram_data_point list ->
  ?aggregation_temporality:aggregation_temporality ->
  unit ->
  exponential_histogram
(** [make_exponential_histogram … ()] is a builder for type [exponential_histogram] *)

val copy_exponential_histogram : exponential_histogram -> exponential_histogram

val set_exponential_histogram_data_points : exponential_histogram -> exponential_histogram_data_point list -> unit
  (** set field data_points in exponential_histogram *)

val has_exponential_histogram_aggregation_temporality : exponential_histogram -> bool
  (** presence of field "aggregation_temporality" in [exponential_histogram] *)

val set_exponential_histogram_aggregation_temporality : exponential_histogram -> aggregation_temporality -> unit
  (** set field aggregation_temporality in exponential_histogram *)

val make_summary_data_point_value_at_quantile : 
  ?quantile:float ->
  ?value:float ->
  unit ->
  summary_data_point_value_at_quantile
(** [make_summary_data_point_value_at_quantile … ()] is a builder for type [summary_data_point_value_at_quantile] *)

val copy_summary_data_point_value_at_quantile : summary_data_point_value_at_quantile -> summary_data_point_value_at_quantile

val has_summary_data_point_value_at_quantile_quantile : summary_data_point_value_at_quantile -> bool
  (** presence of field "quantile" in [summary_data_point_value_at_quantile] *)

val set_summary_data_point_value_at_quantile_quantile : summary_data_point_value_at_quantile -> float -> unit
  (** set field quantile in summary_data_point_value_at_quantile *)

val has_summary_data_point_value_at_quantile_value : summary_data_point_value_at_quantile -> bool
  (** presence of field "value" in [summary_data_point_value_at_quantile] *)

val set_summary_data_point_value_at_quantile_value : summary_data_point_value_at_quantile -> float -> unit
  (** set field value in summary_data_point_value_at_quantile *)

val make_summary_data_point : 
  attributes:Common.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?count:int64 ->
  ?sum:float ->
  quantile_values:summary_data_point_value_at_quantile list ->
  ?flags:int32 ->
  unit ->
  summary_data_point
(** [make_summary_data_point … ()] is a builder for type [summary_data_point] *)

val copy_summary_data_point : summary_data_point -> summary_data_point

val set_summary_data_point_attributes : summary_data_point -> Common.key_value list -> unit
  (** set field attributes in summary_data_point *)

val has_summary_data_point_start_time_unix_nano : summary_data_point -> bool
  (** presence of field "start_time_unix_nano" in [summary_data_point] *)

val set_summary_data_point_start_time_unix_nano : summary_data_point -> int64 -> unit
  (** set field start_time_unix_nano in summary_data_point *)

val has_summary_data_point_time_unix_nano : summary_data_point -> bool
  (** presence of field "time_unix_nano" in [summary_data_point] *)

val set_summary_data_point_time_unix_nano : summary_data_point -> int64 -> unit
  (** set field time_unix_nano in summary_data_point *)

val has_summary_data_point_count : summary_data_point -> bool
  (** presence of field "count" in [summary_data_point] *)

val set_summary_data_point_count : summary_data_point -> int64 -> unit
  (** set field count in summary_data_point *)

val has_summary_data_point_sum : summary_data_point -> bool
  (** presence of field "sum" in [summary_data_point] *)

val set_summary_data_point_sum : summary_data_point -> float -> unit
  (** set field sum in summary_data_point *)

val set_summary_data_point_quantile_values : summary_data_point -> summary_data_point_value_at_quantile list -> unit
  (** set field quantile_values in summary_data_point *)

val has_summary_data_point_flags : summary_data_point -> bool
  (** presence of field "flags" in [summary_data_point] *)

val set_summary_data_point_flags : summary_data_point -> int32 -> unit
  (** set field flags in summary_data_point *)

val make_summary : 
  data_points:summary_data_point list ->
  unit ->
  summary
(** [make_summary … ()] is a builder for type [summary] *)

val copy_summary : summary -> summary

val set_summary_data_points : summary -> summary_data_point list -> unit
  (** set field data_points in summary *)


val make_metric : 
  ?name:string ->
  ?description:string ->
  ?unit_:string ->
  ?data:metric_data ->
  metadata:Common.key_value list ->
  unit ->
  metric
(** [make_metric … ()] is a builder for type [metric] *)

val copy_metric : metric -> metric

val has_metric_name : metric -> bool
  (** presence of field "name" in [metric] *)

val set_metric_name : metric -> string -> unit
  (** set field name in metric *)

val has_metric_description : metric -> bool
  (** presence of field "description" in [metric] *)

val set_metric_description : metric -> string -> unit
  (** set field description in metric *)

val has_metric_unit_ : metric -> bool
  (** presence of field "unit_" in [metric] *)

val set_metric_unit_ : metric -> string -> unit
  (** set field unit_ in metric *)

val set_metric_data : metric -> metric_data -> unit
  (** set field data in metric *)

val set_metric_metadata : metric -> Common.key_value list -> unit
  (** set field metadata in metric *)

val make_scope_metrics : 
  ?scope:Common.instrumentation_scope ->
  metrics:metric list ->
  ?schema_url:string ->
  unit ->
  scope_metrics
(** [make_scope_metrics … ()] is a builder for type [scope_metrics] *)

val copy_scope_metrics : scope_metrics -> scope_metrics

val set_scope_metrics_scope : scope_metrics -> Common.instrumentation_scope -> unit
  (** set field scope in scope_metrics *)

val set_scope_metrics_metrics : scope_metrics -> metric list -> unit
  (** set field metrics in scope_metrics *)

val has_scope_metrics_schema_url : scope_metrics -> bool
  (** presence of field "schema_url" in [scope_metrics] *)

val set_scope_metrics_schema_url : scope_metrics -> string -> unit
  (** set field schema_url in scope_metrics *)

val make_resource_metrics : 
  ?resource:Resource.resource ->
  scope_metrics:scope_metrics list ->
  ?schema_url:string ->
  unit ->
  resource_metrics
(** [make_resource_metrics … ()] is a builder for type [resource_metrics] *)

val copy_resource_metrics : resource_metrics -> resource_metrics

val set_resource_metrics_resource : resource_metrics -> Resource.resource -> unit
  (** set field resource in resource_metrics *)

val set_resource_metrics_scope_metrics : resource_metrics -> scope_metrics list -> unit
  (** set field scope_metrics in resource_metrics *)

val has_resource_metrics_schema_url : resource_metrics -> bool
  (** presence of field "schema_url" in [resource_metrics] *)

val set_resource_metrics_schema_url : resource_metrics -> string -> unit
  (** set field schema_url in resource_metrics *)

val make_metrics_data : 
  resource_metrics:resource_metrics list ->
  unit ->
  metrics_data
(** [make_metrics_data … ()] is a builder for type [metrics_data] *)

val copy_metrics_data : metrics_data -> metrics_data

val set_metrics_data_resource_metrics : metrics_data -> resource_metrics list -> unit
  (** set field resource_metrics in metrics_data *)



(** {2 Formatters} *)

val pp_exemplar_value : Format.formatter -> exemplar_value -> unit 
(** [pp_exemplar_value v] formats v *)

val pp_exemplar : Format.formatter -> exemplar -> unit 
(** [pp_exemplar v] formats v *)

val pp_number_data_point_value : Format.formatter -> number_data_point_value -> unit 
(** [pp_number_data_point_value v] formats v *)

val pp_number_data_point : Format.formatter -> number_data_point -> unit 
(** [pp_number_data_point v] formats v *)

val pp_gauge : Format.formatter -> gauge -> unit 
(** [pp_gauge v] formats v *)

val pp_aggregation_temporality : Format.formatter -> aggregation_temporality -> unit 
(** [pp_aggregation_temporality v] formats v *)

val pp_sum : Format.formatter -> sum -> unit 
(** [pp_sum v] formats v *)

val pp_histogram_data_point : Format.formatter -> histogram_data_point -> unit 
(** [pp_histogram_data_point v] formats v *)

val pp_histogram : Format.formatter -> histogram -> unit 
(** [pp_histogram v] formats v *)

val pp_exponential_histogram_data_point_buckets : Format.formatter -> exponential_histogram_data_point_buckets -> unit 
(** [pp_exponential_histogram_data_point_buckets v] formats v *)

val pp_exponential_histogram_data_point : Format.formatter -> exponential_histogram_data_point -> unit 
(** [pp_exponential_histogram_data_point v] formats v *)

val pp_exponential_histogram : Format.formatter -> exponential_histogram -> unit 
(** [pp_exponential_histogram v] formats v *)

val pp_summary_data_point_value_at_quantile : Format.formatter -> summary_data_point_value_at_quantile -> unit 
(** [pp_summary_data_point_value_at_quantile v] formats v *)

val pp_summary_data_point : Format.formatter -> summary_data_point -> unit 
(** [pp_summary_data_point v] formats v *)

val pp_summary : Format.formatter -> summary -> unit 
(** [pp_summary v] formats v *)

val pp_metric_data : Format.formatter -> metric_data -> unit 
(** [pp_metric_data v] formats v *)

val pp_metric : Format.formatter -> metric -> unit 
(** [pp_metric v] formats v *)

val pp_scope_metrics : Format.formatter -> scope_metrics -> unit 
(** [pp_scope_metrics v] formats v *)

val pp_resource_metrics : Format.formatter -> resource_metrics -> unit 
(** [pp_resource_metrics v] formats v *)

val pp_metrics_data : Format.formatter -> metrics_data -> unit 
(** [pp_metrics_data v] formats v *)

val pp_data_point_flags : Format.formatter -> data_point_flags -> unit 
(** [pp_data_point_flags v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_exemplar_value : exemplar_value -> Pbrt.Encoder.t -> unit
(** [encode_pb_exemplar_value v encoder] encodes [v] with the given [encoder] *)

val encode_pb_exemplar : exemplar -> Pbrt.Encoder.t -> unit
(** [encode_pb_exemplar v encoder] encodes [v] with the given [encoder] *)

val encode_pb_number_data_point_value : number_data_point_value -> Pbrt.Encoder.t -> unit
(** [encode_pb_number_data_point_value v encoder] encodes [v] with the given [encoder] *)

val encode_pb_number_data_point : number_data_point -> Pbrt.Encoder.t -> unit
(** [encode_pb_number_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_pb_gauge : gauge -> Pbrt.Encoder.t -> unit
(** [encode_pb_gauge v encoder] encodes [v] with the given [encoder] *)

val encode_pb_aggregation_temporality : aggregation_temporality -> Pbrt.Encoder.t -> unit
(** [encode_pb_aggregation_temporality v encoder] encodes [v] with the given [encoder] *)

val encode_pb_sum : sum -> Pbrt.Encoder.t -> unit
(** [encode_pb_sum v encoder] encodes [v] with the given [encoder] *)

val encode_pb_histogram_data_point : histogram_data_point -> Pbrt.Encoder.t -> unit
(** [encode_pb_histogram_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_pb_histogram : histogram -> Pbrt.Encoder.t -> unit
(** [encode_pb_histogram v encoder] encodes [v] with the given [encoder] *)

val encode_pb_exponential_histogram_data_point_buckets : exponential_histogram_data_point_buckets -> Pbrt.Encoder.t -> unit
(** [encode_pb_exponential_histogram_data_point_buckets v encoder] encodes [v] with the given [encoder] *)

val encode_pb_exponential_histogram_data_point : exponential_histogram_data_point -> Pbrt.Encoder.t -> unit
(** [encode_pb_exponential_histogram_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_pb_exponential_histogram : exponential_histogram -> Pbrt.Encoder.t -> unit
(** [encode_pb_exponential_histogram v encoder] encodes [v] with the given [encoder] *)

val encode_pb_summary_data_point_value_at_quantile : summary_data_point_value_at_quantile -> Pbrt.Encoder.t -> unit
(** [encode_pb_summary_data_point_value_at_quantile v encoder] encodes [v] with the given [encoder] *)

val encode_pb_summary_data_point : summary_data_point -> Pbrt.Encoder.t -> unit
(** [encode_pb_summary_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_pb_summary : summary -> Pbrt.Encoder.t -> unit
(** [encode_pb_summary v encoder] encodes [v] with the given [encoder] *)

val encode_pb_metric_data : metric_data -> Pbrt.Encoder.t -> unit
(** [encode_pb_metric_data v encoder] encodes [v] with the given [encoder] *)

val encode_pb_metric : metric -> Pbrt.Encoder.t -> unit
(** [encode_pb_metric v encoder] encodes [v] with the given [encoder] *)

val encode_pb_scope_metrics : scope_metrics -> Pbrt.Encoder.t -> unit
(** [encode_pb_scope_metrics v encoder] encodes [v] with the given [encoder] *)

val encode_pb_resource_metrics : resource_metrics -> Pbrt.Encoder.t -> unit
(** [encode_pb_resource_metrics v encoder] encodes [v] with the given [encoder] *)

val encode_pb_metrics_data : metrics_data -> Pbrt.Encoder.t -> unit
(** [encode_pb_metrics_data v encoder] encodes [v] with the given [encoder] *)

val encode_pb_data_point_flags : data_point_flags -> Pbrt.Encoder.t -> unit
(** [encode_pb_data_point_flags v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_exemplar_value : Pbrt.Decoder.t -> exemplar_value
(** [decode_pb_exemplar_value decoder] decodes a [exemplar_value] binary value from [decoder] *)

val decode_pb_exemplar : Pbrt.Decoder.t -> exemplar
(** [decode_pb_exemplar decoder] decodes a [exemplar] binary value from [decoder] *)

val decode_pb_number_data_point_value : Pbrt.Decoder.t -> number_data_point_value
(** [decode_pb_number_data_point_value decoder] decodes a [number_data_point_value] binary value from [decoder] *)

val decode_pb_number_data_point : Pbrt.Decoder.t -> number_data_point
(** [decode_pb_number_data_point decoder] decodes a [number_data_point] binary value from [decoder] *)

val decode_pb_gauge : Pbrt.Decoder.t -> gauge
(** [decode_pb_gauge decoder] decodes a [gauge] binary value from [decoder] *)

val decode_pb_aggregation_temporality : Pbrt.Decoder.t -> aggregation_temporality
(** [decode_pb_aggregation_temporality decoder] decodes a [aggregation_temporality] binary value from [decoder] *)

val decode_pb_sum : Pbrt.Decoder.t -> sum
(** [decode_pb_sum decoder] decodes a [sum] binary value from [decoder] *)

val decode_pb_histogram_data_point : Pbrt.Decoder.t -> histogram_data_point
(** [decode_pb_histogram_data_point decoder] decodes a [histogram_data_point] binary value from [decoder] *)

val decode_pb_histogram : Pbrt.Decoder.t -> histogram
(** [decode_pb_histogram decoder] decodes a [histogram] binary value from [decoder] *)

val decode_pb_exponential_histogram_data_point_buckets : Pbrt.Decoder.t -> exponential_histogram_data_point_buckets
(** [decode_pb_exponential_histogram_data_point_buckets decoder] decodes a [exponential_histogram_data_point_buckets] binary value from [decoder] *)

val decode_pb_exponential_histogram_data_point : Pbrt.Decoder.t -> exponential_histogram_data_point
(** [decode_pb_exponential_histogram_data_point decoder] decodes a [exponential_histogram_data_point] binary value from [decoder] *)

val decode_pb_exponential_histogram : Pbrt.Decoder.t -> exponential_histogram
(** [decode_pb_exponential_histogram decoder] decodes a [exponential_histogram] binary value from [decoder] *)

val decode_pb_summary_data_point_value_at_quantile : Pbrt.Decoder.t -> summary_data_point_value_at_quantile
(** [decode_pb_summary_data_point_value_at_quantile decoder] decodes a [summary_data_point_value_at_quantile] binary value from [decoder] *)

val decode_pb_summary_data_point : Pbrt.Decoder.t -> summary_data_point
(** [decode_pb_summary_data_point decoder] decodes a [summary_data_point] binary value from [decoder] *)

val decode_pb_summary : Pbrt.Decoder.t -> summary
(** [decode_pb_summary decoder] decodes a [summary] binary value from [decoder] *)

val decode_pb_metric_data : Pbrt.Decoder.t -> metric_data
(** [decode_pb_metric_data decoder] decodes a [metric_data] binary value from [decoder] *)

val decode_pb_metric : Pbrt.Decoder.t -> metric
(** [decode_pb_metric decoder] decodes a [metric] binary value from [decoder] *)

val decode_pb_scope_metrics : Pbrt.Decoder.t -> scope_metrics
(** [decode_pb_scope_metrics decoder] decodes a [scope_metrics] binary value from [decoder] *)

val decode_pb_resource_metrics : Pbrt.Decoder.t -> resource_metrics
(** [decode_pb_resource_metrics decoder] decodes a [resource_metrics] binary value from [decoder] *)

val decode_pb_metrics_data : Pbrt.Decoder.t -> metrics_data
(** [decode_pb_metrics_data decoder] decodes a [metrics_data] binary value from [decoder] *)

val decode_pb_data_point_flags : Pbrt.Decoder.t -> data_point_flags
(** [decode_pb_data_point_flags decoder] decodes a [data_point_flags] binary value from [decoder] *)
