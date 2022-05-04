(** trace_service.proto Types *)



(** {2 Types} *)

type export_trace_service_request = {
  resource_spans : Trace_types.resource_spans list;
}


(** {2 Default values} *)

val default_export_trace_service_request : 
  ?resource_spans:Trace_types.resource_spans list ->
  unit ->
  export_trace_service_request
(** [default_export_trace_service_request ()] is the default value for type [export_trace_service_request] *)
