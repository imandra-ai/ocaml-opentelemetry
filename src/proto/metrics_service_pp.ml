[@@@ocaml.warning "-27-30-39"]

let rec pp_export_metrics_service_request fmt (v:Metrics_service_types.export_metrics_service_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_metrics" (Pbrt.Pp.pp_list Metrics_pp.pp_resource_metrics) fmt v.Metrics_service_types.resource_metrics;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_metrics_partial_success fmt (v:Metrics_service_types.export_metrics_partial_success) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "rejected_data_points" Pbrt.Pp.pp_int64 fmt v.Metrics_service_types.rejected_data_points;
    Pbrt.Pp.pp_record_field ~first:false "error_message" Pbrt.Pp.pp_string fmt v.Metrics_service_types.error_message;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_metrics_service_response fmt (v:Metrics_service_types.export_metrics_service_response) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "partial_success" (Pbrt.Pp.pp_option pp_export_metrics_partial_success) fmt v.Metrics_service_types.partial_success;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
