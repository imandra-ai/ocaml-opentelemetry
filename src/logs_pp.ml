[@@@ocaml.warning "-27-30-39"]

let rec pp_severity_number fmt (v:Logs_types.severity_number) =
  match v with
  | Logs_types.Severity_number_unspecified -> Format.fprintf fmt "Severity_number_unspecified"
  | Logs_types.Severity_number_trace -> Format.fprintf fmt "Severity_number_trace"
  | Logs_types.Severity_number_trace2 -> Format.fprintf fmt "Severity_number_trace2"
  | Logs_types.Severity_number_trace3 -> Format.fprintf fmt "Severity_number_trace3"
  | Logs_types.Severity_number_trace4 -> Format.fprintf fmt "Severity_number_trace4"
  | Logs_types.Severity_number_debug -> Format.fprintf fmt "Severity_number_debug"
  | Logs_types.Severity_number_debug2 -> Format.fprintf fmt "Severity_number_debug2"
  | Logs_types.Severity_number_debug3 -> Format.fprintf fmt "Severity_number_debug3"
  | Logs_types.Severity_number_debug4 -> Format.fprintf fmt "Severity_number_debug4"
  | Logs_types.Severity_number_info -> Format.fprintf fmt "Severity_number_info"
  | Logs_types.Severity_number_info2 -> Format.fprintf fmt "Severity_number_info2"
  | Logs_types.Severity_number_info3 -> Format.fprintf fmt "Severity_number_info3"
  | Logs_types.Severity_number_info4 -> Format.fprintf fmt "Severity_number_info4"
  | Logs_types.Severity_number_warn -> Format.fprintf fmt "Severity_number_warn"
  | Logs_types.Severity_number_warn2 -> Format.fprintf fmt "Severity_number_warn2"
  | Logs_types.Severity_number_warn3 -> Format.fprintf fmt "Severity_number_warn3"
  | Logs_types.Severity_number_warn4 -> Format.fprintf fmt "Severity_number_warn4"
  | Logs_types.Severity_number_error -> Format.fprintf fmt "Severity_number_error"
  | Logs_types.Severity_number_error2 -> Format.fprintf fmt "Severity_number_error2"
  | Logs_types.Severity_number_error3 -> Format.fprintf fmt "Severity_number_error3"
  | Logs_types.Severity_number_error4 -> Format.fprintf fmt "Severity_number_error4"
  | Logs_types.Severity_number_fatal -> Format.fprintf fmt "Severity_number_fatal"
  | Logs_types.Severity_number_fatal2 -> Format.fprintf fmt "Severity_number_fatal2"
  | Logs_types.Severity_number_fatal3 -> Format.fprintf fmt "Severity_number_fatal3"
  | Logs_types.Severity_number_fatal4 -> Format.fprintf fmt "Severity_number_fatal4"

let rec pp_log_record fmt (v:Logs_types.log_record) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Logs_types.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "observed_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Logs_types.observed_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "severity_number" pp_severity_number fmt v.Logs_types.severity_number;
    Pbrt.Pp.pp_record_field ~first:false "severity_text" Pbrt.Pp.pp_string fmt v.Logs_types.severity_text;
    Pbrt.Pp.pp_record_field ~first:false "name" Pbrt.Pp.pp_string fmt v.Logs_types.name;
    Pbrt.Pp.pp_record_field ~first:false "body" (Pbrt.Pp.pp_option Common_pp.pp_any_value) fmt v.Logs_types.body;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common_pp.pp_key_value) fmt v.Logs_types.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.Logs_types.dropped_attributes_count;
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.Logs_types.flags;
    Pbrt.Pp.pp_record_field ~first:false "trace_id" Pbrt.Pp.pp_bytes fmt v.Logs_types.trace_id;
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.Logs_types.span_id;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_instrumentation_library_logs fmt (v:Logs_types.instrumentation_library_logs) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "instrumentation_library" (Pbrt.Pp.pp_option Common_pp.pp_instrumentation_library) fmt v.Logs_types.instrumentation_library;
    Pbrt.Pp.pp_record_field ~first:false "log_records" (Pbrt.Pp.pp_list pp_log_record) fmt v.Logs_types.log_records;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.Logs_types.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_resource_logs fmt (v:Logs_types.resource_logs) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource" (Pbrt.Pp.pp_option Resource_pp.pp_resource) fmt v.Logs_types.resource;
    Pbrt.Pp.pp_record_field ~first:false "instrumentation_library_logs" (Pbrt.Pp.pp_list pp_instrumentation_library_logs) fmt v.Logs_types.instrumentation_library_logs;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.Logs_types.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_logs_data fmt (v:Logs_types.logs_data) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_logs" (Pbrt.Pp.pp_list pp_resource_logs) fmt v.Logs_types.resource_logs;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_log_record_flags fmt (v:Logs_types.log_record_flags) =
  match v with
  | Logs_types.Log_record_flag_unspecified -> Format.fprintf fmt "Log_record_flag_unspecified"
  | Logs_types.Log_record_flag_trace_flags_mask -> Format.fprintf fmt "Log_record_flag_trace_flags_mask"
