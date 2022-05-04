(** metrics_service.proto Types *)



(** {2 Types} *)

type export_metrics_service_request = {
  resource_metrics : Metrics_types.resource_metrics list;
}


(** {2 Default values} *)

val default_export_metrics_service_request : 
  ?resource_metrics:Metrics_types.resource_metrics list ->
  unit ->
  export_metrics_service_request
(** [default_export_metrics_service_request ()] is the default value for type [export_metrics_service_request] *)
