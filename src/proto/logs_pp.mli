
(** logs.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_severity_number : Format.formatter -> Logs_types.severity_number -> unit 
(** [pp_severity_number v] formats v *)

val pp_log_record : Format.formatter -> Logs_types.log_record -> unit 
(** [pp_log_record v] formats v *)

val pp_scope_logs : Format.formatter -> Logs_types.scope_logs -> unit 
(** [pp_scope_logs v] formats v *)

val pp_resource_logs : Format.formatter -> Logs_types.resource_logs -> unit 
(** [pp_resource_logs v] formats v *)

val pp_logs_data : Format.formatter -> Logs_types.logs_data -> unit 
(** [pp_logs_data v] formats v *)

val pp_log_record_flags : Format.formatter -> Logs_types.log_record_flags -> unit 
(** [pp_log_record_flags v] formats v *)
