[@@@ocaml.warning "-27-30-39"]

type array_value_mutable = {
  mutable values : Common_types.any_value list;
}

let default_array_value_mutable () : array_value_mutable = {
  values = [];
}

type key_value_list_mutable = {
  mutable values : Common_types.key_value list;
}

let default_key_value_list_mutable () : key_value_list_mutable = {
  values = [];
}

type key_value_mutable = {
  mutable key : string;
  mutable value : Common_types.any_value option;
}

let default_key_value_mutable () : key_value_mutable = {
  key = "";
  value = None;
}

type instrumentation_library_mutable = {
  mutable name : string;
  mutable version : string;
}

let default_instrumentation_library_mutable () : instrumentation_library_mutable = {
  name = "";
  version = "";
}


let rec decode_any_value d = 
  let rec loop () = 
    let ret:Common_types.any_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "any_value"
      | Some (1, _) -> (Common_types.String_value (Pbrt.Decoder.string d) : Common_types.any_value) 
      | Some (2, _) -> (Common_types.Bool_value (Pbrt.Decoder.bool d) : Common_types.any_value) 
      | Some (3, _) -> (Common_types.Int_value (Pbrt.Decoder.int64_as_varint d) : Common_types.any_value) 
      | Some (4, _) -> (Common_types.Double_value (Pbrt.Decoder.float_as_bits64 d) : Common_types.any_value) 
      | Some (5, _) -> (Common_types.Array_value (decode_array_value (Pbrt.Decoder.nested d)) : Common_types.any_value) 
      | Some (6, _) -> (Common_types.Kvlist_value (decode_key_value_list (Pbrt.Decoder.nested d)) : Common_types.any_value) 
      | Some (7, _) -> (Common_types.Bytes_value (Pbrt.Decoder.bytes d) : Common_types.any_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_array_value d =
  let v = default_array_value_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.values <- List.rev v.values;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.values <- (decode_any_value (Pbrt.Decoder.nested d)) :: v.values;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(array_value), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Common_types.values = v.values;
  } : Common_types.array_value)

and decode_key_value_list d =
  let v = default_key_value_list_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.values <- List.rev v.values;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.values <- (decode_key_value (Pbrt.Decoder.nested d)) :: v.values;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(key_value_list), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Common_types.values = v.values;
  } : Common_types.key_value_list)

and decode_key_value d =
  let v = default_key_value_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(key_value), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.value <- Some (decode_any_value (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(key_value), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Common_types.key = v.key;
    Common_types.value = v.value;
  } : Common_types.key_value)

let rec decode_instrumentation_library d =
  let v = default_instrumentation_library_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_library), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.version <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_library), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Common_types.name = v.name;
    Common_types.version = v.version;
  } : Common_types.instrumentation_library)

let rec encode_any_value (v:Common_types.any_value) encoder = 
  begin match v with
  | Common_types.String_value x ->
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Common_types.Bool_value x ->
    Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
    Pbrt.Encoder.bool x encoder;
  | Common_types.Int_value x ->
    Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
    Pbrt.Encoder.int64_as_varint x encoder;
  | Common_types.Double_value x ->
    Pbrt.Encoder.key (4, Pbrt.Bits64) encoder; 
    Pbrt.Encoder.float_as_bits64 x encoder;
  | Common_types.Array_value x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_array_value x) encoder;
  | Common_types.Kvlist_value x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_key_value_list x) encoder;
  | Common_types.Bytes_value x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  end

and encode_array_value (v:Common_types.array_value) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_any_value x) encoder;
  ) v.Common_types.values;
  ()

and encode_key_value_list (v:Common_types.key_value_list) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_key_value x) encoder;
  ) v.Common_types.values;
  ()

and encode_key_value (v:Common_types.key_value) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Common_types.key encoder;
  begin match v.Common_types.value with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_any_value x) encoder;
  | None -> ();
  end;
  ()

let rec encode_instrumentation_library (v:Common_types.instrumentation_library) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Common_types.name encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Common_types.version encoder;
  ()
