[@@@ocaml.warning "-27-30-39"]


type export_metrics_service_request = {
  resource_metrics : Metrics_types.resource_metrics list;
}

let rec default_export_metrics_service_request 
  ?resource_metrics:((resource_metrics:Metrics_types.resource_metrics list) = [])
  () : export_metrics_service_request  = {
  resource_metrics;
}
