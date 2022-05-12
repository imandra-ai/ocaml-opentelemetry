(** logs.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_severity_number : Logs_types.severity_number -> Pbrt.Encoder.t -> unit
(** [encode_severity_number v encoder] encodes [v] with the given [encoder] *)

val encode_log_record : Logs_types.log_record -> Pbrt.Encoder.t -> unit
(** [encode_log_record v encoder] encodes [v] with the given [encoder] *)

val encode_instrumentation_library_logs : Logs_types.instrumentation_library_logs -> Pbrt.Encoder.t -> unit
(** [encode_instrumentation_library_logs v encoder] encodes [v] with the given [encoder] *)

val encode_resource_logs : Logs_types.resource_logs -> Pbrt.Encoder.t -> unit
(** [encode_resource_logs v encoder] encodes [v] with the given [encoder] *)

val encode_logs_data : Logs_types.logs_data -> Pbrt.Encoder.t -> unit
(** [encode_logs_data v encoder] encodes [v] with the given [encoder] *)

val encode_log_record_flags : Logs_types.log_record_flags -> Pbrt.Encoder.t -> unit
(** [encode_log_record_flags v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_severity_number : Pbrt.Decoder.t -> Logs_types.severity_number
(** [decode_severity_number decoder] decodes a [severity_number] value from [decoder] *)

val decode_log_record : Pbrt.Decoder.t -> Logs_types.log_record
(** [decode_log_record decoder] decodes a [log_record] value from [decoder] *)

val decode_instrumentation_library_logs : Pbrt.Decoder.t -> Logs_types.instrumentation_library_logs
(** [decode_instrumentation_library_logs decoder] decodes a [instrumentation_library_logs] value from [decoder] *)

val decode_resource_logs : Pbrt.Decoder.t -> Logs_types.resource_logs
(** [decode_resource_logs decoder] decodes a [resource_logs] value from [decoder] *)

val decode_logs_data : Pbrt.Decoder.t -> Logs_types.logs_data
(** [decode_logs_data decoder] decodes a [logs_data] value from [decoder] *)

val decode_log_record_flags : Pbrt.Decoder.t -> Logs_types.log_record_flags
(** [decode_log_record_flags decoder] decodes a [log_record_flags] value from [decoder] *)
