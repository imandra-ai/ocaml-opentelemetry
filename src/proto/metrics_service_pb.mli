(** metrics_service.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_export_metrics_service_request : Metrics_service_types.export_metrics_service_request -> Pbrt.Encoder.t -> unit
(** [encode_export_metrics_service_request v encoder] encodes [v] with the given [encoder] *)

val encode_export_metrics_partial_success : Metrics_service_types.export_metrics_partial_success -> Pbrt.Encoder.t -> unit
(** [encode_export_metrics_partial_success v encoder] encodes [v] with the given [encoder] *)

val encode_export_metrics_service_response : Metrics_service_types.export_metrics_service_response -> Pbrt.Encoder.t -> unit
(** [encode_export_metrics_service_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_export_metrics_service_request : Pbrt.Decoder.t -> Metrics_service_types.export_metrics_service_request
(** [decode_export_metrics_service_request decoder] decodes a [export_metrics_service_request] value from [decoder] *)

val decode_export_metrics_partial_success : Pbrt.Decoder.t -> Metrics_service_types.export_metrics_partial_success
(** [decode_export_metrics_partial_success decoder] decodes a [export_metrics_partial_success] value from [decoder] *)

val decode_export_metrics_service_response : Pbrt.Decoder.t -> Metrics_service_types.export_metrics_service_response
(** [decode_export_metrics_service_response decoder] decodes a [export_metrics_service_response] value from [decoder] *)
