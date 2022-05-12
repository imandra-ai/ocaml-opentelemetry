(** logs_service.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_export_logs_service_request : Format.formatter -> Logs_service_types.export_logs_service_request -> unit 
(** [pp_export_logs_service_request v] formats v *)
