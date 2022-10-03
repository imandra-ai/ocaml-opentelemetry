(** metrics_service.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_export_metrics_service_request : Format.formatter -> Metrics_service_types.export_metrics_service_request -> unit 
(** [pp_export_metrics_service_request v] formats v *)

val pp_export_metrics_partial_success : Format.formatter -> Metrics_service_types.export_metrics_partial_success -> unit 
(** [pp_export_metrics_partial_success v] formats v *)

val pp_export_metrics_service_response : Format.formatter -> Metrics_service_types.export_metrics_service_response -> unit 
(** [pp_export_metrics_service_response v] formats v *)
