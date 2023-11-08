
(** metrics.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_summary_data_point_value_at_quantile : Format.formatter -> Metrics_types.summary_data_point_value_at_quantile -> unit 
(** [pp_summary_data_point_value_at_quantile v] formats v *)

val pp_summary_data_point : Format.formatter -> Metrics_types.summary_data_point -> unit 
(** [pp_summary_data_point v] formats v *)

val pp_summary : Format.formatter -> Metrics_types.summary -> unit 
(** [pp_summary v] formats v *)

val pp_exponential_histogram_data_point_buckets : Format.formatter -> Metrics_types.exponential_histogram_data_point_buckets -> unit 
(** [pp_exponential_histogram_data_point_buckets v] formats v *)

val pp_exemplar_value : Format.formatter -> Metrics_types.exemplar_value -> unit 
(** [pp_exemplar_value v] formats v *)

val pp_exemplar : Format.formatter -> Metrics_types.exemplar -> unit 
(** [pp_exemplar v] formats v *)

val pp_exponential_histogram_data_point : Format.formatter -> Metrics_types.exponential_histogram_data_point -> unit 
(** [pp_exponential_histogram_data_point v] formats v *)

val pp_aggregation_temporality : Format.formatter -> Metrics_types.aggregation_temporality -> unit 
(** [pp_aggregation_temporality v] formats v *)

val pp_exponential_histogram : Format.formatter -> Metrics_types.exponential_histogram -> unit 
(** [pp_exponential_histogram v] formats v *)

val pp_histogram_data_point : Format.formatter -> Metrics_types.histogram_data_point -> unit 
(** [pp_histogram_data_point v] formats v *)

val pp_histogram : Format.formatter -> Metrics_types.histogram -> unit 
(** [pp_histogram v] formats v *)

val pp_number_data_point_value : Format.formatter -> Metrics_types.number_data_point_value -> unit 
(** [pp_number_data_point_value v] formats v *)

val pp_number_data_point : Format.formatter -> Metrics_types.number_data_point -> unit 
(** [pp_number_data_point v] formats v *)

val pp_sum : Format.formatter -> Metrics_types.sum -> unit 
(** [pp_sum v] formats v *)

val pp_gauge : Format.formatter -> Metrics_types.gauge -> unit 
(** [pp_gauge v] formats v *)

val pp_metric_data : Format.formatter -> Metrics_types.metric_data -> unit 
(** [pp_metric_data v] formats v *)

val pp_metric : Format.formatter -> Metrics_types.metric -> unit 
(** [pp_metric v] formats v *)

val pp_scope_metrics : Format.formatter -> Metrics_types.scope_metrics -> unit 
(** [pp_scope_metrics v] formats v *)

val pp_resource_metrics : Format.formatter -> Metrics_types.resource_metrics -> unit 
(** [pp_resource_metrics v] formats v *)

val pp_metrics_data : Format.formatter -> Metrics_types.metrics_data -> unit 
(** [pp_metrics_data v] formats v *)

val pp_data_point_flags : Format.formatter -> Metrics_types.data_point_flags -> unit 
(** [pp_data_point_flags v] formats v *)
