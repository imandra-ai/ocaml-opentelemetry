[@@@ocaml.warning "-27-30-39"]

let rec pp_export_metrics_service_request fmt (v:Metrics_service_types.export_metrics_service_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_metrics" (Pbrt.Pp.pp_list Metrics_pp.pp_resource_metrics) fmt v.Metrics_service_types.resource_metrics;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
