[@@@ocaml.warning "-27-30-39"]


type export_trace_service_request = {
  resource_spans : Trace_types.resource_spans list;
}

let rec default_export_trace_service_request 
  ?resource_spans:((resource_spans:Trace_types.resource_spans list) = [])
  () : export_trace_service_request  = {
  resource_spans;
}
