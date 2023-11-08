[@@@ocaml.warning "-27-30-39"]

type status_mutable = {
  mutable code : int32;
  mutable message : bytes;
  mutable details : bytes list;
}

let default_status_mutable () : status_mutable = {
  code = 0l;
  message = Bytes.create 0;
  details = [];
}


let rec decode_status d =
  let v = default_status_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.details <- List.rev v.details;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.code <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.message <- Pbrt.Decoder.bytes d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.details <- (Pbrt.Decoder.bytes d) :: v.details;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Status_types.code = v.code;
    Status_types.message = v.message;
    Status_types.details = v.details;
  } : Status_types.status)

let rec encode_status (v:Status_types.status) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Status_types.code encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Status_types.message encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  ) v.Status_types.details;
  ()
