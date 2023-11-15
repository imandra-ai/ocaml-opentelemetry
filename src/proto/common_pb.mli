(** common.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_any_value : Common_types.any_value -> Pbrt.Encoder.t -> unit
(** [encode_any_value v encoder] encodes [v] with the given [encoder] *)

val encode_array_value : Common_types.array_value -> Pbrt.Encoder.t -> unit
(** [encode_array_value v encoder] encodes [v] with the given [encoder] *)

val encode_key_value_list : Common_types.key_value_list -> Pbrt.Encoder.t -> unit
(** [encode_key_value_list v encoder] encodes [v] with the given [encoder] *)

val encode_key_value : Common_types.key_value -> Pbrt.Encoder.t -> unit
(** [encode_key_value v encoder] encodes [v] with the given [encoder] *)

val encode_instrumentation_scope : Common_types.instrumentation_scope -> Pbrt.Encoder.t -> unit
(** [encode_instrumentation_scope v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_any_value : Pbrt.Decoder.t -> Common_types.any_value
(** [decode_any_value decoder] decodes a [any_value] value from [decoder] *)

val decode_array_value : Pbrt.Decoder.t -> Common_types.array_value
(** [decode_array_value decoder] decodes a [array_value] value from [decoder] *)

val decode_key_value_list : Pbrt.Decoder.t -> Common_types.key_value_list
(** [decode_key_value_list decoder] decodes a [key_value_list] value from [decoder] *)

val decode_key_value : Pbrt.Decoder.t -> Common_types.key_value
(** [decode_key_value decoder] decodes a [key_value] value from [decoder] *)

val decode_instrumentation_scope : Pbrt.Decoder.t -> Common_types.instrumentation_scope
(** [decode_instrumentation_scope decoder] decodes a [instrumentation_scope] value from [decoder] *)
