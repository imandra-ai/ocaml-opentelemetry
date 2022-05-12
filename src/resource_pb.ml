[@@@ocaml.warning "-27-30-39"]

type resource_mutable = {
  mutable attributes : Common_types.key_value list;
  mutable dropped_attributes_count : int32;
}

let default_resource_mutable () : resource_mutable = {
  attributes = [];
  dropped_attributes_count = 0l;
}


let rec decode_resource d =
  let v = default_resource_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.attributes <- (Common_pb.decode_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Resource_types.attributes = v.attributes;
    Resource_types.dropped_attributes_count = v.dropped_attributes_count;
  } : Resource_types.resource)

let rec encode_resource (v:Resource_types.resource) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_key_value x) encoder;
  ) v.Resource_types.attributes;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Resource_types.dropped_attributes_count encoder;
  ()
