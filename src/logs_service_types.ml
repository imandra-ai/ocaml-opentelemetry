[@@@ocaml.warning "-27-30-39"]


type export_logs_service_request = {
  resource_logs : Logs_types.resource_logs list;
}

let rec default_export_logs_service_request 
  ?resource_logs:((resource_logs:Logs_types.resource_logs list) = [])
  () : export_logs_service_request  = {
  resource_logs;
}
