(** resource.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_resource : Resource_types.resource -> Pbrt.Encoder.t -> unit
(** [encode_resource v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_resource : Pbrt.Decoder.t -> Resource_types.resource
(** [decode_resource decoder] decodes a [resource] value from [decoder] *)
