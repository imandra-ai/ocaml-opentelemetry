[@@@ocaml.warning "-27-30-39"]


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

let rec default_export_trace_service_request 
  ?resource_spans:((resource_spans:Trace_types.resource_spans list) = [])
  () : export_trace_service_request  = {
  resource_spans;
}

let rec default_export_trace_partial_success 
  ?rejected_spans:((rejected_spans:int64) = 0L)
  ?error_message:((error_message:string) = "")
  () : export_trace_partial_success  = {
  rejected_spans;
  error_message;
}

let rec default_export_trace_service_response 
  ?partial_success:((partial_success:export_trace_partial_success option) = None)
  () : export_trace_service_response  = {
  partial_success;
}
