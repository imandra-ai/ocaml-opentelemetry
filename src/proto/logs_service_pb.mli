(** logs_service.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_export_logs_service_request : Logs_service_types.export_logs_service_request -> Pbrt.Encoder.t -> unit
(** [encode_export_logs_service_request v encoder] encodes [v] with the given [encoder] *)

val encode_export_logs_partial_success : Logs_service_types.export_logs_partial_success -> Pbrt.Encoder.t -> unit
(** [encode_export_logs_partial_success v encoder] encodes [v] with the given [encoder] *)

val encode_export_logs_service_response : Logs_service_types.export_logs_service_response -> Pbrt.Encoder.t -> unit
(** [encode_export_logs_service_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_export_logs_service_request : Pbrt.Decoder.t -> Logs_service_types.export_logs_service_request
(** [decode_export_logs_service_request decoder] decodes a [export_logs_service_request] value from [decoder] *)

val decode_export_logs_partial_success : Pbrt.Decoder.t -> Logs_service_types.export_logs_partial_success
(** [decode_export_logs_partial_success decoder] decodes a [export_logs_partial_success] value from [decoder] *)

val decode_export_logs_service_response : Pbrt.Decoder.t -> Logs_service_types.export_logs_service_response
(** [decode_export_logs_service_response decoder] decodes a [export_logs_service_response] value from [decoder] *)
