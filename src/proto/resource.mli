
(** Code for resource.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/resource/v1/resource.proto", do not edit *)



(** {2 Types} *)

type resource = private {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 1 fields *)
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
  mutable entity_refs : Common.entity_ref list;
}


(** {2 Basic values} *)

val default_resource : unit -> resource 
(** [default_resource ()] is a new empty value for type [resource] *)


(** {2 Make functions} *)

val make_resource : 
  attributes:Common.key_value list ->
  ?dropped_attributes_count:int32 ->
  entity_refs:Common.entity_ref list ->
  unit ->
  resource
(** [make_resource … ()] is a builder for type [resource] *)

val copy_resource : resource -> resource

val set_resource_attributes : resource -> Common.key_value list -> unit
  (** set field attributes in resource *)

val has_resource_dropped_attributes_count : resource -> bool
  (** presence of field "dropped_attributes_count" in [resource] *)

val set_resource_dropped_attributes_count : resource -> int32 -> unit
  (** set field dropped_attributes_count in resource *)

val set_resource_entity_refs : resource -> Common.entity_ref list -> unit
  (** set field entity_refs in resource *)


(** {2 Formatters} *)

val pp_resource : Format.formatter -> resource -> unit 
(** [pp_resource v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_resource : resource -> Pbrt.Encoder.t -> unit
(** [encode_pb_resource v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_resource : Pbrt.Decoder.t -> resource
(** [decode_pb_resource decoder] decodes a [resource] binary value from [decoder] *)
