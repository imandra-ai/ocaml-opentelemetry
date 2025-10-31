[@@@ocaml.warning "-23-27-30-39-44"]

type resource = {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 1 fields *)
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
  mutable entity_refs : Common.entity_ref list;
}

let default_resource (): resource = 
{
  _presence=Pbrt.Bitfield.empty;
  attributes=[];
  dropped_attributes_count=0l;
  entity_refs=[];
}


(** {2 Make functions} *)

let[@inline] has_resource_dropped_attributes_count (self:resource) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] set_resource_attributes (self:resource) (x:Common.key_value list) : unit =
  self.attributes <- x
let[@inline] set_resource_dropped_attributes_count (self:resource) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.dropped_attributes_count <- x
let[@inline] set_resource_entity_refs (self:resource) (x:Common.entity_ref list) : unit =
  self.entity_refs <- x

let copy_resource (self:resource) : resource =
  { self with attributes = self.attributes }

let make_resource 
  ~(attributes:Common.key_value list) 
  ?(dropped_attributes_count:int32 option)
  ~(entity_refs:Common.entity_ref list) 
  () : resource  =
  let _res = default_resource () in
  set_resource_attributes _res attributes;
  (match dropped_attributes_count with
  | None -> ()
  | Some v -> set_resource_dropped_attributes_count _res v);
  set_resource_entity_refs _res entity_refs;
  _res

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Formatters} *)

let rec pp_resource fmt (v:resource) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "entity_refs" (Pbrt.Pp.pp_list Common.pp_entity_ref) fmt v.entity_refs;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_resource (v:resource) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_entity_ref x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.entity_refs encoder;
  ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_resource d =
  let v = default_resource () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_resource_entity_refs v (List.rev v.entity_refs);
      set_resource_attributes v (List.rev v.attributes);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_resource_attributes v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      set_resource_dropped_attributes_count v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      set_resource_entity_refs v ((Common.decode_pb_entity_ref (Pbrt.Decoder.nested d)) :: v.entity_refs);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : resource)
