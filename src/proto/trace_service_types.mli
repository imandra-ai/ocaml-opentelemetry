(** trace_service.proto Types *)



(** {2 Types} *)

type export_trace_service_request = {
  resource_spans : Trace_types.resource_spans list;
}

type export_trace_partial_success = {
  rejected_spans : int64;
  error_message : string;
}

type export_trace_service_response = {
  partial_success : export_trace_partial_success option;
}


(** {2 Default values} *)

val default_export_trace_service_request : 
  ?resource_spans:Trace_types.resource_spans list ->
  unit ->
  export_trace_service_request
(** [default_export_trace_service_request ()] is the default value for type [export_trace_service_request] *)

val default_export_trace_partial_success : 
  ?rejected_spans:int64 ->
  ?error_message:string ->
  unit ->
  export_trace_partial_success
(** [default_export_trace_partial_success ()] is the default value for type [export_trace_partial_success] *)

val default_export_trace_service_response : 
  ?partial_success:export_trace_partial_success option ->
  unit ->
  export_trace_service_response
(** [default_export_trace_service_response ()] is the default value for type [export_trace_service_response] *)
