(** status.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_status : Status_types.status -> Pbrt.Encoder.t -> unit
(** [encode_status v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_status : Pbrt.Decoder.t -> Status_types.status
(** [decode_status decoder] decodes a [status] value from [decoder] *)
