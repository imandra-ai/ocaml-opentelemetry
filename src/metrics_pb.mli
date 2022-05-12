(** metrics.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_exemplar_value : Metrics_types.exemplar_value -> Pbrt.Encoder.t -> unit
(** [encode_exemplar_value v encoder] encodes [v] with the given [encoder] *)

val encode_exemplar : Metrics_types.exemplar -> Pbrt.Encoder.t -> unit
(** [encode_exemplar v encoder] encodes [v] with the given [encoder] *)

val encode_number_data_point_value : Metrics_types.number_data_point_value -> Pbrt.Encoder.t -> unit
(** [encode_number_data_point_value v encoder] encodes [v] with the given [encoder] *)

val encode_number_data_point : Metrics_types.number_data_point -> Pbrt.Encoder.t -> unit
(** [encode_number_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_gauge : Metrics_types.gauge -> Pbrt.Encoder.t -> unit
(** [encode_gauge v encoder] encodes [v] with the given [encoder] *)

val encode_aggregation_temporality : Metrics_types.aggregation_temporality -> Pbrt.Encoder.t -> unit
(** [encode_aggregation_temporality v encoder] encodes [v] with the given [encoder] *)

val encode_sum : Metrics_types.sum -> Pbrt.Encoder.t -> unit
(** [encode_sum v encoder] encodes [v] with the given [encoder] *)

val encode_histogram_data_point : Metrics_types.histogram_data_point -> Pbrt.Encoder.t -> unit
(** [encode_histogram_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_histogram : Metrics_types.histogram -> Pbrt.Encoder.t -> unit
(** [encode_histogram v encoder] encodes [v] with the given [encoder] *)

val encode_exponential_histogram_data_point_buckets : Metrics_types.exponential_histogram_data_point_buckets -> Pbrt.Encoder.t -> unit
(** [encode_exponential_histogram_data_point_buckets v encoder] encodes [v] with the given [encoder] *)

val encode_exponential_histogram_data_point : Metrics_types.exponential_histogram_data_point -> Pbrt.Encoder.t -> unit
(** [encode_exponential_histogram_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_exponential_histogram : Metrics_types.exponential_histogram -> Pbrt.Encoder.t -> unit
(** [encode_exponential_histogram v encoder] encodes [v] with the given [encoder] *)

val encode_summary_data_point_value_at_quantile : Metrics_types.summary_data_point_value_at_quantile -> Pbrt.Encoder.t -> unit
(** [encode_summary_data_point_value_at_quantile v encoder] encodes [v] with the given [encoder] *)

val encode_summary_data_point : Metrics_types.summary_data_point -> Pbrt.Encoder.t -> unit
(** [encode_summary_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_summary : Metrics_types.summary -> Pbrt.Encoder.t -> unit
(** [encode_summary v encoder] encodes [v] with the given [encoder] *)

val encode_metric_data : Metrics_types.metric_data -> Pbrt.Encoder.t -> unit
(** [encode_metric_data v encoder] encodes [v] with the given [encoder] *)

val encode_metric : Metrics_types.metric -> Pbrt.Encoder.t -> unit
(** [encode_metric v encoder] encodes [v] with the given [encoder] *)

val encode_instrumentation_library_metrics : Metrics_types.instrumentation_library_metrics -> Pbrt.Encoder.t -> unit
(** [encode_instrumentation_library_metrics v encoder] encodes [v] with the given [encoder] *)

val encode_resource_metrics : Metrics_types.resource_metrics -> Pbrt.Encoder.t -> unit
(** [encode_resource_metrics v encoder] encodes [v] with the given [encoder] *)

val encode_metrics_data : Metrics_types.metrics_data -> Pbrt.Encoder.t -> unit
(** [encode_metrics_data v encoder] encodes [v] with the given [encoder] *)

val encode_data_point_flags : Metrics_types.data_point_flags -> Pbrt.Encoder.t -> unit
(** [encode_data_point_flags v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_exemplar_value : Pbrt.Decoder.t -> Metrics_types.exemplar_value
(** [decode_exemplar_value decoder] decodes a [exemplar_value] value from [decoder] *)

val decode_exemplar : Pbrt.Decoder.t -> Metrics_types.exemplar
(** [decode_exemplar decoder] decodes a [exemplar] value from [decoder] *)

val decode_number_data_point_value : Pbrt.Decoder.t -> Metrics_types.number_data_point_value
(** [decode_number_data_point_value decoder] decodes a [number_data_point_value] value from [decoder] *)

val decode_number_data_point : Pbrt.Decoder.t -> Metrics_types.number_data_point
(** [decode_number_data_point decoder] decodes a [number_data_point] value from [decoder] *)

val decode_gauge : Pbrt.Decoder.t -> Metrics_types.gauge
(** [decode_gauge decoder] decodes a [gauge] value from [decoder] *)

val decode_aggregation_temporality : Pbrt.Decoder.t -> Metrics_types.aggregation_temporality
(** [decode_aggregation_temporality decoder] decodes a [aggregation_temporality] value from [decoder] *)

val decode_sum : Pbrt.Decoder.t -> Metrics_types.sum
(** [decode_sum decoder] decodes a [sum] value from [decoder] *)

val decode_histogram_data_point : Pbrt.Decoder.t -> Metrics_types.histogram_data_point
(** [decode_histogram_data_point decoder] decodes a [histogram_data_point] value from [decoder] *)

val decode_histogram : Pbrt.Decoder.t -> Metrics_types.histogram
(** [decode_histogram decoder] decodes a [histogram] value from [decoder] *)

val decode_exponential_histogram_data_point_buckets : Pbrt.Decoder.t -> Metrics_types.exponential_histogram_data_point_buckets
(** [decode_exponential_histogram_data_point_buckets decoder] decodes a [exponential_histogram_data_point_buckets] value from [decoder] *)

val decode_exponential_histogram_data_point : Pbrt.Decoder.t -> Metrics_types.exponential_histogram_data_point
(** [decode_exponential_histogram_data_point decoder] decodes a [exponential_histogram_data_point] value from [decoder] *)

val decode_exponential_histogram : Pbrt.Decoder.t -> Metrics_types.exponential_histogram
(** [decode_exponential_histogram decoder] decodes a [exponential_histogram] value from [decoder] *)

val decode_summary_data_point_value_at_quantile : Pbrt.Decoder.t -> Metrics_types.summary_data_point_value_at_quantile
(** [decode_summary_data_point_value_at_quantile decoder] decodes a [summary_data_point_value_at_quantile] value from [decoder] *)

val decode_summary_data_point : Pbrt.Decoder.t -> Metrics_types.summary_data_point
(** [decode_summary_data_point decoder] decodes a [summary_data_point] value from [decoder] *)

val decode_summary : Pbrt.Decoder.t -> Metrics_types.summary
(** [decode_summary decoder] decodes a [summary] value from [decoder] *)

val decode_metric_data : Pbrt.Decoder.t -> Metrics_types.metric_data
(** [decode_metric_data decoder] decodes a [metric_data] value from [decoder] *)

val decode_metric : Pbrt.Decoder.t -> Metrics_types.metric
(** [decode_metric decoder] decodes a [metric] value from [decoder] *)

val decode_instrumentation_library_metrics : Pbrt.Decoder.t -> Metrics_types.instrumentation_library_metrics
(** [decode_instrumentation_library_metrics decoder] decodes a [instrumentation_library_metrics] value from [decoder] *)

val decode_resource_metrics : Pbrt.Decoder.t -> Metrics_types.resource_metrics
(** [decode_resource_metrics decoder] decodes a [resource_metrics] value from [decoder] *)

val decode_metrics_data : Pbrt.Decoder.t -> Metrics_types.metrics_data
(** [decode_metrics_data decoder] decodes a [metrics_data] value from [decoder] *)

val decode_data_point_flags : Pbrt.Decoder.t -> Metrics_types.data_point_flags
(** [decode_data_point_flags decoder] decodes a [data_point_flags] value from [decoder] *)
