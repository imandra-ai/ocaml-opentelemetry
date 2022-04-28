[@@@ocaml.warning "-27-30-39"]

let rec pp_exemplar_value fmt (v:Metrics_types.exemplar_value) =
  match v with
  | Metrics_types.As_double x -> Format.fprintf fmt "@[<hv2>As_double(@,%a)@]" Pbrt.Pp.pp_float x
  | Metrics_types.As_int x -> Format.fprintf fmt "@[<hv2>As_int(@,%a)@]" Pbrt.Pp.pp_int64 x

and pp_exemplar fmt (v:Metrics_types.exemplar) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "filtered_attributes" (Pbrt.Pp.pp_list Common_pp.pp_key_value) fmt v.Metrics_types.filtered_attributes;
    Pbrt.Pp.pp_record_field ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Metrics_types.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "value" pp_exemplar_value fmt v.Metrics_types.value;
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.Metrics_types.span_id;
    Pbrt.Pp.pp_record_field ~first:false "trace_id" Pbrt.Pp.pp_bytes fmt v.Metrics_types.trace_id;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_number_data_point_value fmt (v:Metrics_types.number_data_point_value) =
  match v with
  | Metrics_types.As_double x -> Format.fprintf fmt "@[<hv2>As_double(@,%a)@]" Pbrt.Pp.pp_float x
  | Metrics_types.As_int x -> Format.fprintf fmt "@[<hv2>As_int(@,%a)@]" Pbrt.Pp.pp_int64 x

and pp_number_data_point fmt (v:Metrics_types.number_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common_pp.pp_key_value) fmt v.Metrics_types.attributes;
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Metrics_types.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Metrics_types.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "value" pp_number_data_point_value fmt v.Metrics_types.value;
    Pbrt.Pp.pp_record_field ~first:false "exemplars" (Pbrt.Pp.pp_list pp_exemplar) fmt v.Metrics_types.exemplars;
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.Metrics_types.flags;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_gauge fmt (v:Metrics_types.gauge) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_number_data_point) fmt v.Metrics_types.data_points;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_aggregation_temporality fmt (v:Metrics_types.aggregation_temporality) =
  match v with
  | Metrics_types.Aggregation_temporality_unspecified -> Format.fprintf fmt "Aggregation_temporality_unspecified"
  | Metrics_types.Aggregation_temporality_delta -> Format.fprintf fmt "Aggregation_temporality_delta"
  | Metrics_types.Aggregation_temporality_cumulative -> Format.fprintf fmt "Aggregation_temporality_cumulative"

let rec pp_sum fmt (v:Metrics_types.sum) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_number_data_point) fmt v.Metrics_types.data_points;
    Pbrt.Pp.pp_record_field ~first:false "aggregation_temporality" pp_aggregation_temporality fmt v.Metrics_types.aggregation_temporality;
    Pbrt.Pp.pp_record_field ~first:false "is_monotonic" Pbrt.Pp.pp_bool fmt v.Metrics_types.is_monotonic;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_histogram_data_point fmt (v:Metrics_types.histogram_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common_pp.pp_key_value) fmt v.Metrics_types.attributes;
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Metrics_types.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Metrics_types.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "count" Pbrt.Pp.pp_int64 fmt v.Metrics_types.count;
    Pbrt.Pp.pp_record_field ~first:false "sum" Pbrt.Pp.pp_float fmt v.Metrics_types.sum;
    Pbrt.Pp.pp_record_field ~first:false "bucket_counts" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64) fmt v.Metrics_types.bucket_counts;
    Pbrt.Pp.pp_record_field ~first:false "explicit_bounds" (Pbrt.Pp.pp_list Pbrt.Pp.pp_float) fmt v.Metrics_types.explicit_bounds;
    Pbrt.Pp.pp_record_field ~first:false "exemplars" (Pbrt.Pp.pp_list pp_exemplar) fmt v.Metrics_types.exemplars;
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.Metrics_types.flags;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_histogram fmt (v:Metrics_types.histogram) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_histogram_data_point) fmt v.Metrics_types.data_points;
    Pbrt.Pp.pp_record_field ~first:false "aggregation_temporality" pp_aggregation_temporality fmt v.Metrics_types.aggregation_temporality;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_exponential_histogram_data_point_buckets fmt (v:Metrics_types.exponential_histogram_data_point_buckets) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "offset" Pbrt.Pp.pp_int32 fmt v.Metrics_types.offset;
    Pbrt.Pp.pp_record_field ~first:false "bucket_counts" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64) fmt v.Metrics_types.bucket_counts;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_exponential_histogram_data_point fmt (v:Metrics_types.exponential_histogram_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common_pp.pp_key_value) fmt v.Metrics_types.attributes;
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Metrics_types.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Metrics_types.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "count" Pbrt.Pp.pp_int64 fmt v.Metrics_types.count;
    Pbrt.Pp.pp_record_field ~first:false "sum" Pbrt.Pp.pp_float fmt v.Metrics_types.sum;
    Pbrt.Pp.pp_record_field ~first:false "scale" Pbrt.Pp.pp_int32 fmt v.Metrics_types.scale;
    Pbrt.Pp.pp_record_field ~first:false "zero_count" Pbrt.Pp.pp_int64 fmt v.Metrics_types.zero_count;
    Pbrt.Pp.pp_record_field ~first:false "positive" (Pbrt.Pp.pp_option pp_exponential_histogram_data_point_buckets) fmt v.Metrics_types.positive;
    Pbrt.Pp.pp_record_field ~first:false "negative" (Pbrt.Pp.pp_option pp_exponential_histogram_data_point_buckets) fmt v.Metrics_types.negative;
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.Metrics_types.flags;
    Pbrt.Pp.pp_record_field ~first:false "exemplars" (Pbrt.Pp.pp_list pp_exemplar) fmt v.Metrics_types.exemplars;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_exponential_histogram fmt (v:Metrics_types.exponential_histogram) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_exponential_histogram_data_point) fmt v.Metrics_types.data_points;
    Pbrt.Pp.pp_record_field ~first:false "aggregation_temporality" pp_aggregation_temporality fmt v.Metrics_types.aggregation_temporality;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_data_point_value_at_quantile fmt (v:Metrics_types.summary_data_point_value_at_quantile) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "quantile" Pbrt.Pp.pp_float fmt v.Metrics_types.quantile;
    Pbrt.Pp.pp_record_field ~first:false "value" Pbrt.Pp.pp_float fmt v.Metrics_types.value;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_data_point fmt (v:Metrics_types.summary_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common_pp.pp_key_value) fmt v.Metrics_types.attributes;
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Metrics_types.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Metrics_types.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "count" Pbrt.Pp.pp_int64 fmt v.Metrics_types.count;
    Pbrt.Pp.pp_record_field ~first:false "sum" Pbrt.Pp.pp_float fmt v.Metrics_types.sum;
    Pbrt.Pp.pp_record_field ~first:false "quantile_values" (Pbrt.Pp.pp_list pp_summary_data_point_value_at_quantile) fmt v.Metrics_types.quantile_values;
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.Metrics_types.flags;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary fmt (v:Metrics_types.summary) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_summary_data_point) fmt v.Metrics_types.data_points;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_metric_data fmt (v:Metrics_types.metric_data) =
  match v with
  | Metrics_types.Gauge x -> Format.fprintf fmt "@[<hv2>Gauge(@,%a)@]" pp_gauge x
  | Metrics_types.Sum x -> Format.fprintf fmt "@[<hv2>Sum(@,%a)@]" pp_sum x
  | Metrics_types.Histogram x -> Format.fprintf fmt "@[<hv2>Histogram(@,%a)@]" pp_histogram x
  | Metrics_types.Exponential_histogram x -> Format.fprintf fmt "@[<hv2>Exponential_histogram(@,%a)@]" pp_exponential_histogram x
  | Metrics_types.Summary x -> Format.fprintf fmt "@[<hv2>Summary(@,%a)@]" pp_summary x

and pp_metric fmt (v:Metrics_types.metric) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.Metrics_types.name;
    Pbrt.Pp.pp_record_field ~first:false "description" Pbrt.Pp.pp_string fmt v.Metrics_types.description;
    Pbrt.Pp.pp_record_field ~first:false "unit_" Pbrt.Pp.pp_string fmt v.Metrics_types.unit_;
    Pbrt.Pp.pp_record_field ~first:false "data" pp_metric_data fmt v.Metrics_types.data;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_instrumentation_library_metrics fmt (v:Metrics_types.instrumentation_library_metrics) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "instrumentation_library" (Pbrt.Pp.pp_option Common_pp.pp_instrumentation_library) fmt v.Metrics_types.instrumentation_library;
    Pbrt.Pp.pp_record_field ~first:false "metrics" (Pbrt.Pp.pp_list pp_metric) fmt v.Metrics_types.metrics;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.Metrics_types.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_resource_metrics fmt (v:Metrics_types.resource_metrics) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource" (Pbrt.Pp.pp_option Resource_pp.pp_resource) fmt v.Metrics_types.resource;
    Pbrt.Pp.pp_record_field ~first:false "instrumentation_library_metrics" (Pbrt.Pp.pp_list pp_instrumentation_library_metrics) fmt v.Metrics_types.instrumentation_library_metrics;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.Metrics_types.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_metrics_data fmt (v:Metrics_types.metrics_data) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_metrics" (Pbrt.Pp.pp_list pp_resource_metrics) fmt v.Metrics_types.resource_metrics;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_data_point_flags fmt (v:Metrics_types.data_point_flags) =
  match v with
  | Metrics_types.Flag_none -> Format.fprintf fmt "Flag_none"
  | Metrics_types.Flag_no_recorded_value -> Format.fprintf fmt "Flag_no_recorded_value"
