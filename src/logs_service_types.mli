(** logs_service.proto Types *)



(** {2 Types} *)

type export_logs_service_request = {
  resource_logs : Logs_types.resource_logs list;
}


(** {2 Default values} *)

val default_export_logs_service_request : 
  ?resource_logs:Logs_types.resource_logs list ->
  unit ->
  export_logs_service_request
(** [default_export_logs_service_request ()] is the default value for type [export_logs_service_request] *)
