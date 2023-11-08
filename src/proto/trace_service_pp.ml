[@@@ocaml.warning "-27-30-39"]

let rec pp_export_trace_service_request fmt (v:Trace_service_types.export_trace_service_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_spans" (Pbrt.Pp.pp_list Trace_pp.pp_resource_spans) fmt v.Trace_service_types.resource_spans;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_trace_partial_success fmt (v:Trace_service_types.export_trace_partial_success) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "rejected_spans" Pbrt.Pp.pp_int64 fmt v.Trace_service_types.rejected_spans;
    Pbrt.Pp.pp_record_field ~first:false "error_message" Pbrt.Pp.pp_string fmt v.Trace_service_types.error_message;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_trace_service_response fmt (v:Trace_service_types.export_trace_service_response) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "partial_success" (Pbrt.Pp.pp_option pp_export_trace_partial_success) fmt v.Trace_service_types.partial_success;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
