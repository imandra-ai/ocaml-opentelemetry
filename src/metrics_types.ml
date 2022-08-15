[@@@ocaml.warning "-27-30-39"]


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
  sum : float option;
  bucket_counts : int64 list;
  explicit_bounds : float list;
  exemplars : exemplar list;
  flags : int32;
  min : float option;
  max : float option;
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
  sum : float option;
  scale : int32;
  zero_count : int64;
  positive : exponential_histogram_data_point_buckets option;
  negative : exponential_histogram_data_point_buckets option;
  flags : int32;
  exemplars : exemplar list;
  min : float option;
  max : float option;
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

type scope_metrics = {
  scope : Common_types.instrumentation_scope option;
  metrics : metric list;
  schema_url : string;
}

type resource_metrics = {
  resource : Resource_types.resource option;
  scope_metrics : scope_metrics list;
  schema_url : string;
}

type metrics_data = {
  resource_metrics : resource_metrics list;
}

type data_point_flags =
  | Flag_none 
  | Flag_no_recorded_value 

let rec default_exemplar_value () : exemplar_value = As_double (0.)

and default_exemplar 
  ?filtered_attributes:((filtered_attributes:Common_types.key_value list) = [])
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?value:((value:exemplar_value) = As_double (0.))
  ?span_id:((span_id:bytes) = Bytes.create 0)
  ?trace_id:((trace_id:bytes) = Bytes.create 0)
  () : exemplar  = {
  filtered_attributes;
  time_unix_nano;
  value;
  span_id;
  trace_id;
}

let rec default_number_data_point_value () : number_data_point_value = As_double (0.)

and default_number_data_point 
  ?attributes:((attributes:Common_types.key_value list) = [])
  ?start_time_unix_nano:((start_time_unix_nano:int64) = 0L)
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?value:((value:number_data_point_value) = As_double (0.))
  ?exemplars:((exemplars:exemplar list) = [])
  ?flags:((flags:int32) = 0l)
  () : number_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  value;
  exemplars;
  flags;
}

let rec default_gauge 
  ?data_points:((data_points:number_data_point list) = [])
  () : gauge  = {
  data_points;
}

let rec default_aggregation_temporality () = (Aggregation_temporality_unspecified:aggregation_temporality)

let rec default_sum 
  ?data_points:((data_points:number_data_point list) = [])
  ?aggregation_temporality:((aggregation_temporality:aggregation_temporality) = default_aggregation_temporality ())
  ?is_monotonic:((is_monotonic:bool) = false)
  () : sum  = {
  data_points;
  aggregation_temporality;
  is_monotonic;
}

let rec default_histogram_data_point 
  ?attributes:((attributes:Common_types.key_value list) = [])
  ?start_time_unix_nano:((start_time_unix_nano:int64) = 0L)
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?count:((count:int64) = 0L)
  ?sum:((sum:float option) = None)
  ?bucket_counts:((bucket_counts:int64 list) = [])
  ?explicit_bounds:((explicit_bounds:float list) = [])
  ?exemplars:((exemplars:exemplar list) = [])
  ?flags:((flags:int32) = 0l)
  ?min:((min:float option) = None)
  ?max:((max:float option) = None)
  () : histogram_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  count;
  sum;
  bucket_counts;
  explicit_bounds;
  exemplars;
  flags;
  min;
  max;
}

let rec default_histogram 
  ?data_points:((data_points:histogram_data_point list) = [])
  ?aggregation_temporality:((aggregation_temporality:aggregation_temporality) = default_aggregation_temporality ())
  () : histogram  = {
  data_points;
  aggregation_temporality;
}

let rec default_exponential_histogram_data_point_buckets 
  ?offset:((offset:int32) = 0l)
  ?bucket_counts:((bucket_counts:int64 list) = [])
  () : exponential_histogram_data_point_buckets  = {
  offset;
  bucket_counts;
}

let rec default_exponential_histogram_data_point 
  ?attributes:((attributes:Common_types.key_value list) = [])
  ?start_time_unix_nano:((start_time_unix_nano:int64) = 0L)
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?count:((count:int64) = 0L)
  ?sum:((sum:float option) = None)
  ?scale:((scale:int32) = 0l)
  ?zero_count:((zero_count:int64) = 0L)
  ?positive:((positive:exponential_histogram_data_point_buckets option) = None)
  ?negative:((negative:exponential_histogram_data_point_buckets option) = None)
  ?flags:((flags:int32) = 0l)
  ?exemplars:((exemplars:exemplar list) = [])
  ?min:((min:float option) = None)
  ?max:((max:float option) = None)
  () : exponential_histogram_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  count;
  sum;
  scale;
  zero_count;
  positive;
  negative;
  flags;
  exemplars;
  min;
  max;
}

let rec default_exponential_histogram 
  ?data_points:((data_points:exponential_histogram_data_point list) = [])
  ?aggregation_temporality:((aggregation_temporality:aggregation_temporality) = default_aggregation_temporality ())
  () : exponential_histogram  = {
  data_points;
  aggregation_temporality;
}

let rec default_summary_data_point_value_at_quantile 
  ?quantile:((quantile:float) = 0.)
  ?value:((value:float) = 0.)
  () : summary_data_point_value_at_quantile  = {
  quantile;
  value;
}

let rec default_summary_data_point 
  ?attributes:((attributes:Common_types.key_value list) = [])
  ?start_time_unix_nano:((start_time_unix_nano:int64) = 0L)
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?count:((count:int64) = 0L)
  ?sum:((sum:float) = 0.)
  ?quantile_values:((quantile_values:summary_data_point_value_at_quantile list) = [])
  ?flags:((flags:int32) = 0l)
  () : summary_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  count;
  sum;
  quantile_values;
  flags;
}

let rec default_summary 
  ?data_points:((data_points:summary_data_point list) = [])
  () : summary  = {
  data_points;
}

let rec default_metric_data () : metric_data = Gauge (default_gauge ())

and default_metric 
  ?name:((name:string) = "")
  ?description:((description:string) = "")
  ?unit_:((unit_:string) = "")
  ?data:((data:metric_data) = Gauge (default_gauge ()))
  () : metric  = {
  name;
  description;
  unit_;
  data;
}

let rec default_scope_metrics 
  ?scope:((scope:Common_types.instrumentation_scope option) = None)
  ?metrics:((metrics:metric list) = [])
  ?schema_url:((schema_url:string) = "")
  () : scope_metrics  = {
  scope;
  metrics;
  schema_url;
}

let rec default_resource_metrics 
  ?resource:((resource:Resource_types.resource option) = None)
  ?scope_metrics:((scope_metrics:scope_metrics list) = [])
  ?schema_url:((schema_url:string) = "")
  () : resource_metrics  = {
  resource;
  scope_metrics;
  schema_url;
}

let rec default_metrics_data 
  ?resource_metrics:((resource_metrics:resource_metrics list) = [])
  () : metrics_data  = {
  resource_metrics;
}

let rec default_data_point_flags () = (Flag_none:data_point_flags)
