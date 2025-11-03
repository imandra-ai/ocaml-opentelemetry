
(** Code for logs.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/logs/v1/logs.proto", do not edit *)



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

type log_record = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 9 fields *)
  mutable time_unix_nano : int64;
  mutable observed_time_unix_nano : int64;
  mutable severity_number : severity_number;
  mutable severity_text : string;
  mutable body : Common.any_value option;
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
  mutable flags : int32;
  mutable trace_id : bytes;
  mutable span_id : bytes;
  mutable event_name : string;
}

type scope_logs = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable scope : Common.instrumentation_scope option;
  mutable log_records : log_record list;
  mutable schema_url : string;
}

type resource_logs = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable resource : Resource.resource option;
  mutable scope_logs : scope_logs list;
  mutable schema_url : string;
}

type logs_data = private {
  mutable resource_logs : resource_logs list;
}

type log_record_flags =
  | Log_record_flags_do_not_use 
  | Log_record_flags_trace_flags_mask 


(** {2 Basic values} *)

val default_severity_number : unit -> severity_number
(** [default_severity_number ()] is a new empty value for type [severity_number] *)

val default_log_record : unit -> log_record 
(** [default_log_record ()] is a new empty value for type [log_record] *)

val default_scope_logs : unit -> scope_logs 
(** [default_scope_logs ()] is a new empty value for type [scope_logs] *)

val default_resource_logs : unit -> resource_logs 
(** [default_resource_logs ()] is a new empty value for type [resource_logs] *)

val default_logs_data : unit -> logs_data 
(** [default_logs_data ()] is a new empty value for type [logs_data] *)

val default_log_record_flags : unit -> log_record_flags
(** [default_log_record_flags ()] is a new empty value for type [log_record_flags] *)


(** {2 Make functions} *)


val make_log_record : 
  ?time_unix_nano:int64 ->
  ?observed_time_unix_nano:int64 ->
  ?severity_number:severity_number ->
  ?severity_text:string ->
  ?body:Common.any_value ->
  ?attributes:Common.key_value list ->
  ?dropped_attributes_count:int32 ->
  ?flags:int32 ->
  ?trace_id:bytes ->
  ?span_id:bytes ->
  ?event_name:string ->
  unit ->
  log_record
(** [make_log_record … ()] is a builder for type [log_record] *)

val copy_log_record : log_record -> log_record

val has_log_record_time_unix_nano : log_record -> bool
  (** presence of field "time_unix_nano" in [log_record] *)

val set_log_record_time_unix_nano : log_record -> int64 -> unit
  (** set field time_unix_nano in log_record *)

val has_log_record_observed_time_unix_nano : log_record -> bool
  (** presence of field "observed_time_unix_nano" in [log_record] *)

val set_log_record_observed_time_unix_nano : log_record -> int64 -> unit
  (** set field observed_time_unix_nano in log_record *)

val has_log_record_severity_number : log_record -> bool
  (** presence of field "severity_number" in [log_record] *)

val set_log_record_severity_number : log_record -> severity_number -> unit
  (** set field severity_number in log_record *)

val has_log_record_severity_text : log_record -> bool
  (** presence of field "severity_text" in [log_record] *)

val set_log_record_severity_text : log_record -> string -> unit
  (** set field severity_text in log_record *)

val set_log_record_body : log_record -> Common.any_value -> unit
  (** set field body in log_record *)

val set_log_record_attributes : log_record -> Common.key_value list -> unit
  (** set field attributes in log_record *)

val has_log_record_dropped_attributes_count : log_record -> bool
  (** presence of field "dropped_attributes_count" in [log_record] *)

val set_log_record_dropped_attributes_count : log_record -> int32 -> unit
  (** set field dropped_attributes_count in log_record *)

val has_log_record_flags : log_record -> bool
  (** presence of field "flags" in [log_record] *)

val set_log_record_flags : log_record -> int32 -> unit
  (** set field flags in log_record *)

val has_log_record_trace_id : log_record -> bool
  (** presence of field "trace_id" in [log_record] *)

val set_log_record_trace_id : log_record -> bytes -> unit
  (** set field trace_id in log_record *)

val has_log_record_span_id : log_record -> bool
  (** presence of field "span_id" in [log_record] *)

val set_log_record_span_id : log_record -> bytes -> unit
  (** set field span_id in log_record *)

val has_log_record_event_name : log_record -> bool
  (** presence of field "event_name" in [log_record] *)

val set_log_record_event_name : log_record -> string -> unit
  (** set field event_name in log_record *)

val make_scope_logs : 
  ?scope:Common.instrumentation_scope ->
  ?log_records:log_record list ->
  ?schema_url:string ->
  unit ->
  scope_logs
(** [make_scope_logs … ()] is a builder for type [scope_logs] *)

val copy_scope_logs : scope_logs -> scope_logs

val set_scope_logs_scope : scope_logs -> Common.instrumentation_scope -> unit
  (** set field scope in scope_logs *)

val set_scope_logs_log_records : scope_logs -> log_record list -> unit
  (** set field log_records in scope_logs *)

val has_scope_logs_schema_url : scope_logs -> bool
  (** presence of field "schema_url" in [scope_logs] *)

val set_scope_logs_schema_url : scope_logs -> string -> unit
  (** set field schema_url in scope_logs *)

val make_resource_logs : 
  ?resource:Resource.resource ->
  ?scope_logs:scope_logs list ->
  ?schema_url:string ->
  unit ->
  resource_logs
(** [make_resource_logs … ()] is a builder for type [resource_logs] *)

val copy_resource_logs : resource_logs -> resource_logs

val set_resource_logs_resource : resource_logs -> Resource.resource -> unit
  (** set field resource in resource_logs *)

val set_resource_logs_scope_logs : resource_logs -> scope_logs list -> unit
  (** set field scope_logs in resource_logs *)

val has_resource_logs_schema_url : resource_logs -> bool
  (** presence of field "schema_url" in [resource_logs] *)

val set_resource_logs_schema_url : resource_logs -> string -> unit
  (** set field schema_url in resource_logs *)

val make_logs_data : 
  ?resource_logs:resource_logs list ->
  unit ->
  logs_data
(** [make_logs_data … ()] is a builder for type [logs_data] *)

val copy_logs_data : logs_data -> logs_data

val set_logs_data_resource_logs : logs_data -> resource_logs list -> unit
  (** set field resource_logs in logs_data *)



(** {2 Formatters} *)

val pp_severity_number : Format.formatter -> severity_number -> unit 
(** [pp_severity_number v] formats v *)

val pp_log_record : Format.formatter -> log_record -> unit 
(** [pp_log_record v] formats v *)

val pp_scope_logs : Format.formatter -> scope_logs -> unit 
(** [pp_scope_logs v] formats v *)

val pp_resource_logs : Format.formatter -> resource_logs -> unit 
(** [pp_resource_logs v] formats v *)

val pp_logs_data : Format.formatter -> logs_data -> unit 
(** [pp_logs_data v] formats v *)

val pp_log_record_flags : Format.formatter -> log_record_flags -> unit 
(** [pp_log_record_flags v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_severity_number : severity_number -> Pbrt.Encoder.t -> unit
(** [encode_pb_severity_number v encoder] encodes [v] with the given [encoder] *)

val encode_pb_log_record : log_record -> Pbrt.Encoder.t -> unit
(** [encode_pb_log_record v encoder] encodes [v] with the given [encoder] *)

val encode_pb_scope_logs : scope_logs -> Pbrt.Encoder.t -> unit
(** [encode_pb_scope_logs v encoder] encodes [v] with the given [encoder] *)

val encode_pb_resource_logs : resource_logs -> Pbrt.Encoder.t -> unit
(** [encode_pb_resource_logs v encoder] encodes [v] with the given [encoder] *)

val encode_pb_logs_data : logs_data -> Pbrt.Encoder.t -> unit
(** [encode_pb_logs_data v encoder] encodes [v] with the given [encoder] *)

val encode_pb_log_record_flags : log_record_flags -> Pbrt.Encoder.t -> unit
(** [encode_pb_log_record_flags v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_severity_number : Pbrt.Decoder.t -> severity_number
(** [decode_pb_severity_number decoder] decodes a [severity_number] binary value from [decoder] *)

val decode_pb_log_record : Pbrt.Decoder.t -> log_record
(** [decode_pb_log_record decoder] decodes a [log_record] binary value from [decoder] *)

val decode_pb_scope_logs : Pbrt.Decoder.t -> scope_logs
(** [decode_pb_scope_logs decoder] decodes a [scope_logs] binary value from [decoder] *)

val decode_pb_resource_logs : Pbrt.Decoder.t -> resource_logs
(** [decode_pb_resource_logs decoder] decodes a [resource_logs] binary value from [decoder] *)

val decode_pb_logs_data : Pbrt.Decoder.t -> logs_data
(** [decode_pb_logs_data decoder] decodes a [logs_data] binary value from [decoder] *)

val decode_pb_log_record_flags : Pbrt.Decoder.t -> log_record_flags
(** [decode_pb_log_record_flags decoder] decodes a [log_record_flags] binary value from [decoder] *)
