[@@@ocaml.warning "-27-30-39"]

let rec pp_export_trace_service_request fmt (v:Trace_service_types.export_trace_service_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_spans" (Pbrt.Pp.pp_list Trace_pp.pp_resource_spans) fmt v.Trace_service_types.resource_spans;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
