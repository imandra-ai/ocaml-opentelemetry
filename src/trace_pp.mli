(** trace.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_span_span_kind : Format.formatter -> Trace_types.span_span_kind -> unit 
(** [pp_span_span_kind v] formats v *)

val pp_span_event : Format.formatter -> Trace_types.span_event -> unit 
(** [pp_span_event v] formats v *)

val pp_span_link : Format.formatter -> Trace_types.span_link -> unit 
(** [pp_span_link v] formats v *)

val pp_status_status_code : Format.formatter -> Trace_types.status_status_code -> unit 
(** [pp_status_status_code v] formats v *)

val pp_status : Format.formatter -> Trace_types.status -> unit 
(** [pp_status v] formats v *)

val pp_span : Format.formatter -> Trace_types.span -> unit 
(** [pp_span v] formats v *)

val pp_instrumentation_library_spans : Format.formatter -> Trace_types.instrumentation_library_spans -> unit 
(** [pp_instrumentation_library_spans v] formats v *)

val pp_resource_spans : Format.formatter -> Trace_types.resource_spans -> unit 
(** [pp_resource_spans v] formats v *)

val pp_traces_data : Format.formatter -> Trace_types.traces_data -> unit 
(** [pp_traces_data v] formats v *)
