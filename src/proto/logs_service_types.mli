(** logs_service.proto Types *)



(** {2 Types} *)

type export_logs_service_request = {
  resource_logs : Logs_types.resource_logs list;
}

type export_logs_partial_success = {
  rejected_log_records : int64;
  error_message : string;
}

type export_logs_service_response = {
  partial_success : export_logs_partial_success option;
}


(** {2 Default values} *)

val default_export_logs_service_request : 
  ?resource_logs:Logs_types.resource_logs list ->
  unit ->
  export_logs_service_request
(** [default_export_logs_service_request ()] is the default value for type [export_logs_service_request] *)

val default_export_logs_partial_success : 
  ?rejected_log_records:int64 ->
  ?error_message:string ->
  unit ->
  export_logs_partial_success
(** [default_export_logs_partial_success ()] is the default value for type [export_logs_partial_success] *)

val default_export_logs_service_response : 
  ?partial_success:export_logs_partial_success option ->
  unit ->
  export_logs_service_response
(** [default_export_logs_service_response ()] is the default value for type [export_logs_service_response] *)
