(** trace.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_span_span_kind : Trace_types.span_span_kind -> Pbrt.Encoder.t -> unit
(** [encode_span_span_kind v encoder] encodes [v] with the given [encoder] *)

val encode_span_event : Trace_types.span_event -> Pbrt.Encoder.t -> unit
(** [encode_span_event v encoder] encodes [v] with the given [encoder] *)

val encode_span_link : Trace_types.span_link -> Pbrt.Encoder.t -> unit
(** [encode_span_link v encoder] encodes [v] with the given [encoder] *)

val encode_status_status_code : Trace_types.status_status_code -> Pbrt.Encoder.t -> unit
(** [encode_status_status_code v encoder] encodes [v] with the given [encoder] *)

val encode_status : Trace_types.status -> Pbrt.Encoder.t -> unit
(** [encode_status v encoder] encodes [v] with the given [encoder] *)

val encode_span : Trace_types.span -> Pbrt.Encoder.t -> unit
(** [encode_span v encoder] encodes [v] with the given [encoder] *)

val encode_instrumentation_library_spans : Trace_types.instrumentation_library_spans -> Pbrt.Encoder.t -> unit
(** [encode_instrumentation_library_spans v encoder] encodes [v] with the given [encoder] *)

val encode_resource_spans : Trace_types.resource_spans -> Pbrt.Encoder.t -> unit
(** [encode_resource_spans v encoder] encodes [v] with the given [encoder] *)

val encode_traces_data : Trace_types.traces_data -> Pbrt.Encoder.t -> unit
(** [encode_traces_data v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_span_span_kind : Pbrt.Decoder.t -> Trace_types.span_span_kind
(** [decode_span_span_kind decoder] decodes a [span_span_kind] value from [decoder] *)

val decode_span_event : Pbrt.Decoder.t -> Trace_types.span_event
(** [decode_span_event decoder] decodes a [span_event] value from [decoder] *)

val decode_span_link : Pbrt.Decoder.t -> Trace_types.span_link
(** [decode_span_link decoder] decodes a [span_link] value from [decoder] *)

val decode_status_status_code : Pbrt.Decoder.t -> Trace_types.status_status_code
(** [decode_status_status_code decoder] decodes a [status_status_code] value from [decoder] *)

val decode_status : Pbrt.Decoder.t -> Trace_types.status
(** [decode_status decoder] decodes a [status] value from [decoder] *)

val decode_span : Pbrt.Decoder.t -> Trace_types.span
(** [decode_span decoder] decodes a [span] value from [decoder] *)

val decode_instrumentation_library_spans : Pbrt.Decoder.t -> Trace_types.instrumentation_library_spans
(** [decode_instrumentation_library_spans decoder] decodes a [instrumentation_library_spans] value from [decoder] *)

val decode_resource_spans : Pbrt.Decoder.t -> Trace_types.resource_spans
(** [decode_resource_spans decoder] decodes a [resource_spans] value from [decoder] *)

val decode_traces_data : Pbrt.Decoder.t -> Trace_types.traces_data
(** [decode_traces_data decoder] decodes a [traces_data] value from [decoder] *)
