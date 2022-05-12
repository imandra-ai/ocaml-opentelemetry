(** metrics_service.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_export_metrics_service_request : Format.formatter -> Metrics_service_types.export_metrics_service_request -> unit 
(** [pp_export_metrics_service_request v] formats v *)
