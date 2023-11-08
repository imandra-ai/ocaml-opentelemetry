
(** trace_service.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_export_trace_service_request : Format.formatter -> Trace_service_types.export_trace_service_request -> unit 
(** [pp_export_trace_service_request v] formats v *)

val pp_export_trace_partial_success : Format.formatter -> Trace_service_types.export_trace_partial_success -> unit 
(** [pp_export_trace_partial_success v] formats v *)

val pp_export_trace_service_response : Format.formatter -> Trace_service_types.export_trace_service_response -> unit 
(** [pp_export_trace_service_response v] formats v *)
