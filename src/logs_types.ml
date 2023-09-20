[@@@ocaml.warning "-27-30-39"]


type severity_number =
  | Severity_number_unspecified 
  | Severity_number_trace 
  | Severity_number_trace2 
  | Severity_number_trace3 
  | Severity_number_trace4 
  | Severity_number_debug 
  | Severity_number_debug2 
  | Severity_number_debug3 
  | Severity_number_debug4 
  | Severity_number_info 
  | Severity_number_info2 
  | Severity_number_info3 
  | Severity_number_info4 
  | Severity_number_warn 
  | Severity_number_warn2 
  | Severity_number_warn3 
  | Severity_number_warn4 
  | Severity_number_error 
  | Severity_number_error2 
  | Severity_number_error3 
  | Severity_number_error4 
  | Severity_number_fatal 
  | Severity_number_fatal2 
  | Severity_number_fatal3 
  | Severity_number_fatal4 

type log_record = {
  time_unix_nano : int64;
  observed_time_unix_nano : int64;
  severity_number : severity_number;
  severity_text : string;
  body : Common_types.any_value option;
  attributes : Common_types.key_value list;
  dropped_attributes_count : int32;
  flags : int32;
  trace_id : bytes;
  span_id : bytes;
}

type scope_logs = {
  scope : Common_types.instrumentation_scope option;
  log_records : log_record list;
  schema_url : string;
}

type resource_logs = {
  resource : Resource_types.resource option;
  scope_logs : scope_logs list;
  schema_url : string;
}

type logs_data = {
  resource_logs : resource_logs list;
}

type log_record_flags =
  | Log_record_flags_do_not_use 
  | Log_record_flags_trace_flags_mask 

let rec default_severity_number () = (Severity_number_unspecified:severity_number)

let rec default_log_record 
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?observed_time_unix_nano:((observed_time_unix_nano:int64) = 0L)
  ?severity_number:((severity_number:severity_number) = default_severity_number ())
  ?severity_text:((severity_text:string) = "")
  ?body:((body:Common_types.any_value option) = None)
  ?attributes:((attributes:Common_types.key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  ?flags:((flags:int32) = 0l)
  ?trace_id:((trace_id:bytes) = Bytes.create 0)
  ?span_id:((span_id:bytes) = Bytes.create 0)
  () : log_record  = {
  time_unix_nano;
  observed_time_unix_nano;
  severity_number;
  severity_text;
  body;
  attributes;
  dropped_attributes_count;
  flags;
  trace_id;
  span_id;
}

let rec default_scope_logs 
  ?scope:((scope:Common_types.instrumentation_scope option) = None)
  ?log_records:((log_records:log_record list) = [])
  ?schema_url:((schema_url:string) = "")
  () : scope_logs  = {
  scope;
  log_records;
  schema_url;
}

let rec default_resource_logs 
  ?resource:((resource:Resource_types.resource option) = None)
  ?scope_logs:((scope_logs:scope_logs list) = [])
  ?schema_url:((schema_url:string) = "")
  () : resource_logs  = {
  resource;
  scope_logs;
  schema_url;
}

let rec default_logs_data 
  ?resource_logs:((resource_logs:resource_logs list) = [])
  () : logs_data  = {
  resource_logs;
}

let rec default_log_record_flags () = (Log_record_flags_do_not_use:log_record_flags)
