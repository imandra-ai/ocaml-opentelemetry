(** logs.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_severity_number : unit -> severity_number
(** [default_severity_number ()] is the default value for type [severity_number] *)

val default_log_record : 
  ?time_unix_nano:int64 ->
  ?observed_time_unix_nano:int64 ->
  ?severity_number:severity_number ->
  ?severity_text:string ->
  ?body:Common_types.any_value option ->
  ?attributes:Common_types.key_value list ->
  ?dropped_attributes_count:int32 ->
  ?flags:int32 ->
  ?trace_id:bytes ->
  ?span_id:bytes ->
  unit ->
  log_record
(** [default_log_record ()] is the default value for type [log_record] *)

val default_scope_logs : 
  ?scope:Common_types.instrumentation_scope option ->
  ?log_records:log_record list ->
  ?schema_url:string ->
  unit ->
  scope_logs
(** [default_scope_logs ()] is the default value for type [scope_logs] *)

val default_resource_logs : 
  ?resource:Resource_types.resource option ->
  ?scope_logs:scope_logs list ->
  ?schema_url:string ->
  unit ->
  resource_logs
(** [default_resource_logs ()] is the default value for type [resource_logs] *)

val default_logs_data : 
  ?resource_logs:resource_logs list ->
  unit ->
  logs_data
(** [default_logs_data ()] is the default value for type [logs_data] *)

val default_log_record_flags : unit -> log_record_flags
(** [default_log_record_flags ()] is the default value for type [log_record_flags] *)
