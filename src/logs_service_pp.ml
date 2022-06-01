[@@@ocaml.warning "-27-30-39"]

let rec pp_export_logs_service_request fmt (v:Logs_service_types.export_logs_service_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_logs" (Pbrt.Pp.pp_list Logs_pp.pp_resource_logs) fmt v.Logs_service_types.resource_logs;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
