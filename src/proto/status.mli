
(** Code for status.proto *)

(* generated from "status.proto", do not edit *)



(** {2 Types} *)

type status = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable code : int32;
  mutable message : bytes;
  mutable details : bytes list;
}


(** {2 Basic values} *)

val default_status : unit -> status 
(** [default_status ()] is a new empty value for type [status] *)


(** {2 Make functions} *)

val make_status : 
  ?code:int32 ->
  ?message:bytes ->
  ?details:bytes list ->
  unit ->
  status
(** [make_status … ()] is a builder for type [status] *)

val copy_status : status -> status

val has_status_code : status -> bool
  (** presence of field "code" in [status] *)

val set_status_code : status -> int32 -> unit
  (** set field code in status *)

val has_status_message : status -> bool
  (** presence of field "message" in [status] *)

val set_status_message : status -> bytes -> unit
  (** set field message in status *)

val set_status_details : status -> bytes list -> unit
  (** set field details in status *)


(** {2 Formatters} *)

val pp_status : Format.formatter -> status -> unit 
(** [pp_status v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_status : status -> Pbrt.Encoder.t -> unit
(** [encode_pb_status v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_status : Pbrt.Decoder.t -> status
(** [decode_pb_status decoder] decodes a [status] binary value from [decoder] *)
