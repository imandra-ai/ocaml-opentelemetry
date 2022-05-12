(** metrics_service.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_export_metrics_service_request : Metrics_service_types.export_metrics_service_request -> Pbrt.Encoder.t -> unit
(** [encode_export_metrics_service_request v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_export_metrics_service_request : Pbrt.Decoder.t -> Metrics_service_types.export_metrics_service_request
(** [decode_export_metrics_service_request decoder] decodes a [export_metrics_service_request] value from [decoder] *)
