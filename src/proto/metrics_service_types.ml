[@@@ocaml.warning "-27-30-39"]


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

let rec default_export_metrics_service_request 
  ?resource_metrics:((resource_metrics:Metrics_types.resource_metrics list) = [])
  () : export_metrics_service_request  = {
  resource_metrics;
}

let rec default_export_metrics_partial_success 
  ?rejected_data_points:((rejected_data_points:int64) = 0L)
  ?error_message:((error_message:string) = "")
  () : export_metrics_partial_success  = {
  rejected_data_points;
  error_message;
}

let rec default_export_metrics_service_response 
  ?partial_success:((partial_success:export_metrics_partial_success option) = None)
  () : export_metrics_service_response  = {
  partial_success;
}
