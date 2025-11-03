
(** Code for trace_service.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/collector/trace/v1/trace_service.proto", do not edit *)



(** {2 Types} *)

type export_trace_service_request = private {
  mutable resource_spans : Trace.resource_spans list;
}

type export_trace_partial_success = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable rejected_spans : int64;
  mutable error_message : string;
}

type export_trace_service_response = private {
  mutable partial_success : export_trace_partial_success option;
}


(** {2 Basic values} *)

val default_export_trace_service_request : unit -> export_trace_service_request 
(** [default_export_trace_service_request ()] is a new empty value for type [export_trace_service_request] *)

val default_export_trace_partial_success : unit -> export_trace_partial_success 
(** [default_export_trace_partial_success ()] is a new empty value for type [export_trace_partial_success] *)

val default_export_trace_service_response : unit -> export_trace_service_response 
(** [default_export_trace_service_response ()] is a new empty value for type [export_trace_service_response] *)


(** {2 Make functions} *)

val make_export_trace_service_request : 
  ?resource_spans:Trace.resource_spans list ->
  unit ->
  export_trace_service_request
(** [make_export_trace_service_request … ()] is a builder for type [export_trace_service_request] *)

val copy_export_trace_service_request : export_trace_service_request -> export_trace_service_request

val set_export_trace_service_request_resource_spans : export_trace_service_request -> Trace.resource_spans list -> unit
  (** set field resource_spans in export_trace_service_request *)

val make_export_trace_partial_success : 
  ?rejected_spans:int64 ->
  ?error_message:string ->
  unit ->
  export_trace_partial_success
(** [make_export_trace_partial_success … ()] is a builder for type [export_trace_partial_success] *)

val copy_export_trace_partial_success : export_trace_partial_success -> export_trace_partial_success

val has_export_trace_partial_success_rejected_spans : export_trace_partial_success -> bool
  (** presence of field "rejected_spans" in [export_trace_partial_success] *)

val set_export_trace_partial_success_rejected_spans : export_trace_partial_success -> int64 -> unit
  (** set field rejected_spans in export_trace_partial_success *)

val has_export_trace_partial_success_error_message : export_trace_partial_success -> bool
  (** presence of field "error_message" in [export_trace_partial_success] *)

val set_export_trace_partial_success_error_message : export_trace_partial_success -> string -> unit
  (** set field error_message in export_trace_partial_success *)

val make_export_trace_service_response : 
  ?partial_success:export_trace_partial_success ->
  unit ->
  export_trace_service_response
(** [make_export_trace_service_response … ()] is a builder for type [export_trace_service_response] *)

val copy_export_trace_service_response : export_trace_service_response -> export_trace_service_response

val set_export_trace_service_response_partial_success : export_trace_service_response -> export_trace_partial_success -> unit
  (** set field partial_success in export_trace_service_response *)


(** {2 Formatters} *)

val pp_export_trace_service_request : Format.formatter -> export_trace_service_request -> unit 
(** [pp_export_trace_service_request v] formats v *)

val pp_export_trace_partial_success : Format.formatter -> export_trace_partial_success -> unit 
(** [pp_export_trace_partial_success v] formats v *)

val pp_export_trace_service_response : Format.formatter -> export_trace_service_response -> unit 
(** [pp_export_trace_service_response v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_export_trace_service_request : export_trace_service_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_export_trace_service_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_export_trace_partial_success : export_trace_partial_success -> Pbrt.Encoder.t -> unit
(** [encode_pb_export_trace_partial_success v encoder] encodes [v] with the given [encoder] *)

val encode_pb_export_trace_service_response : export_trace_service_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_export_trace_service_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_export_trace_service_request : Pbrt.Decoder.t -> export_trace_service_request
(** [decode_pb_export_trace_service_request decoder] decodes a [export_trace_service_request] binary value from [decoder] *)

val decode_pb_export_trace_partial_success : Pbrt.Decoder.t -> export_trace_partial_success
(** [decode_pb_export_trace_partial_success decoder] decodes a [export_trace_partial_success] binary value from [decoder] *)

val decode_pb_export_trace_service_response : Pbrt.Decoder.t -> export_trace_service_response
(** [decode_pb_export_trace_service_response decoder] decodes a [export_trace_service_response] binary value from [decoder] *)
