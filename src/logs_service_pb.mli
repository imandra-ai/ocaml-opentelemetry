(** logs_service.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_export_logs_service_request : Logs_service_types.export_logs_service_request -> Pbrt.Encoder.t -> unit
(** [encode_export_logs_service_request v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_export_logs_service_request : Pbrt.Decoder.t -> Logs_service_types.export_logs_service_request
(** [decode_export_logs_service_request decoder] decodes a [export_logs_service_request] value from [decoder] *)
