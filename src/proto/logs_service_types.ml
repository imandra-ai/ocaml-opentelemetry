[@@@ocaml.warning "-27-30-39"]


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

let rec default_export_logs_service_request 
  ?resource_logs:((resource_logs:Logs_types.resource_logs list) = [])
  () : export_logs_service_request  = {
  resource_logs;
}

let rec default_export_logs_partial_success 
  ?rejected_log_records:((rejected_log_records:int64) = 0L)
  ?error_message:((error_message:string) = "")
  () : export_logs_partial_success  = {
  rejected_log_records;
  error_message;
}

let rec default_export_logs_service_response 
  ?partial_success:((partial_success:export_logs_partial_success option) = None)
  () : export_logs_service_response  = {
  partial_success;
}
