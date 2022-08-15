(** metrics_service.proto Types *)



(** {2 Types} *)

type export_metrics_service_request = {
  resource_metrics : Metrics_types.resource_metrics list;
}

type export_metrics_partial_success = {
  rejected_data_points : int64;
  error_message : string;
}

type export_metrics_service_response = {
  partial_success : export_metrics_partial_success option;
}


(** {2 Default values} *)

val default_export_metrics_service_request : 
  ?resource_metrics:Metrics_types.resource_metrics list ->
  unit ->
  export_metrics_service_request
(** [default_export_metrics_service_request ()] is the default value for type [export_metrics_service_request] *)

val default_export_metrics_partial_success : 
  ?rejected_data_points:int64 ->
  ?error_message:string ->
  unit ->
  export_metrics_partial_success
(** [default_export_metrics_partial_success ()] is the default value for type [export_metrics_partial_success] *)

val default_export_metrics_service_response : 
  ?partial_success:export_metrics_partial_success option ->
  unit ->
  export_metrics_service_response
(** [default_export_metrics_service_response ()] is the default value for type [export_metrics_service_response] *)
