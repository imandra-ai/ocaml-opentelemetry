[@@@ocaml.warning "-23-27-30-39-44"]

type any_value =
  | String_value of string
  | Bool_value of bool
  | Int_value of int64
  | Double_value of float
  | Array_value of array_value
  | Kvlist_value of key_value_list
  | Bytes_value of bytes

and array_value = {
  mutable values : any_value list;
}

and key_value_list = {
  mutable values : key_value list;
}

and key_value = {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 1 fields *)
  mutable key : string;
  mutable value : any_value option;
}

type instrumentation_scope = {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 3 fields *)
  mutable name : string;
  mutable version : string;
  mutable attributes : key_value list;
  mutable dropped_attributes_count : int32;
}

type entity_ref = {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 2 fields *)
  mutable schema_url : string;
  mutable type_ : string;
  mutable id_keys : string list;
  mutable description_keys : string list;
}

let default_any_value (): any_value = String_value ("")

let default_array_value (): array_value = 
{
  values=[];
}

let default_key_value_list (): key_value_list = 
{
  values=[];
}

let default_key_value (): key_value = 
{
  _presence=Pbrt.Bitfield.empty;
  key="";
  value=None;
}

let default_instrumentation_scope (): instrumentation_scope = 
{
  _presence=Pbrt.Bitfield.empty;
  name="";
  version="";
  attributes=[];
  dropped_attributes_count=0l;
}

let default_entity_ref (): entity_ref = 
{
  _presence=Pbrt.Bitfield.empty;
  schema_url="";
  type_="";
  id_keys=[];
  description_keys=[];
}


(** {2 Make functions} *)



let[@inline] set_array_value_values (self:array_value) (x:any_value list) : unit =
  self.values <- x

let copy_array_value (self:array_value) : array_value =
  { self with values = self.values }

let make_array_value 
  ~(values:any_value list) 
  () : array_value  =
  let _res = default_array_value () in
  set_array_value_values _res values;
  _res


let[@inline] set_key_value_list_values (self:key_value_list) (x:key_value list) : unit =
  self.values <- x

let copy_key_value_list (self:key_value_list) : key_value_list =
  { self with values = self.values }

let make_key_value_list 
  ~(values:key_value list) 
  () : key_value_list  =
  let _res = default_key_value_list () in
  set_key_value_list_values _res values;
  _res

let[@inline] has_key_value_key (self:key_value) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] set_key_value_key (self:key_value) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.key <- x
let[@inline] set_key_value_value (self:key_value) (x:any_value) : unit =
  self.value <- Some x

let copy_key_value (self:key_value) : key_value =
  { self with key = self.key }

let make_key_value 
  ?(key:string option)
  ?(value:any_value option)
  () : key_value  =
  let _res = default_key_value () in
  (match key with
  | None -> ()
  | Some v -> set_key_value_key _res v);
  (match value with
  | None -> ()
  | Some v -> set_key_value_value _res v);
  _res

let[@inline] has_instrumentation_scope_name (self:instrumentation_scope) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] has_instrumentation_scope_version (self:instrumentation_scope) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] has_instrumentation_scope_dropped_attributes_count (self:instrumentation_scope) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] set_instrumentation_scope_name (self:instrumentation_scope) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.name <- x
let[@inline] set_instrumentation_scope_version (self:instrumentation_scope) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.version <- x
let[@inline] set_instrumentation_scope_attributes (self:instrumentation_scope) (x:key_value list) : unit =
  self.attributes <- x
let[@inline] set_instrumentation_scope_dropped_attributes_count (self:instrumentation_scope) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.dropped_attributes_count <- x

let copy_instrumentation_scope (self:instrumentation_scope) : instrumentation_scope =
  { self with name = self.name }

let make_instrumentation_scope 
  ?(name:string option)
  ?(version:string option)
  ~(attributes:key_value list) 
  ?(dropped_attributes_count:int32 option)
  () : instrumentation_scope  =
  let _res = default_instrumentation_scope () in
  (match name with
  | None -> ()
  | Some v -> set_instrumentation_scope_name _res v);
  (match version with
  | None -> ()
  | Some v -> set_instrumentation_scope_version _res v);
  set_instrumentation_scope_attributes _res attributes;
  (match dropped_attributes_count with
  | None -> ()
  | Some v -> set_instrumentation_scope_dropped_attributes_count _res v);
  _res

let[@inline] has_entity_ref_schema_url (self:entity_ref) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] has_entity_ref_type_ (self:entity_ref) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] set_entity_ref_schema_url (self:entity_ref) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.schema_url <- x
let[@inline] set_entity_ref_type_ (self:entity_ref) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.type_ <- x
let[@inline] set_entity_ref_id_keys (self:entity_ref) (x:string list) : unit =
  self.id_keys <- x
let[@inline] set_entity_ref_description_keys (self:entity_ref) (x:string list) : unit =
  self.description_keys <- x

let copy_entity_ref (self:entity_ref) : entity_ref =
  { self with schema_url = self.schema_url }

let make_entity_ref 
  ?(schema_url:string option)
  ?(type_:string option)
  ~(id_keys:string list) 
  ~(description_keys:string list) 
  () : entity_ref  =
  let _res = default_entity_ref () in
  (match schema_url with
  | None -> ()
  | Some v -> set_entity_ref_schema_url _res v);
  (match type_ with
  | None -> ()
  | Some v -> set_entity_ref_type_ _res v);
  set_entity_ref_id_keys _res id_keys;
  set_entity_ref_description_keys _res description_keys;
  _res

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Formatters} *)

let rec pp_any_value fmt (v:any_value) =
  match v with
  | String_value x -> Format.fprintf fmt "@[<hv2>String_value(@,%a)@]" Pbrt.Pp.pp_string x
  | Bool_value x -> Format.fprintf fmt "@[<hv2>Bool_value(@,%a)@]" Pbrt.Pp.pp_bool x
  | Int_value x -> Format.fprintf fmt "@[<hv2>Int_value(@,%a)@]" Pbrt.Pp.pp_int64 x
  | Double_value x -> Format.fprintf fmt "@[<hv2>Double_value(@,%a)@]" Pbrt.Pp.pp_float x
  | Array_value x -> Format.fprintf fmt "@[<hv2>Array_value(@,%a)@]" pp_array_value x
  | Kvlist_value x -> Format.fprintf fmt "@[<hv2>Kvlist_value(@,%a)@]" pp_key_value_list x
  | Bytes_value x -> Format.fprintf fmt "@[<hv2>Bytes_value(@,%a)@]" Pbrt.Pp.pp_bytes x

and pp_array_value fmt (v:array_value) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "values" (Pbrt.Pp.pp_list pp_any_value) fmt v.values;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_key_value_list fmt (v:key_value_list) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "values" (Pbrt.Pp.pp_list pp_key_value) fmt v.values;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_key_value fmt (v:key_value) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "key" Pbrt.Pp.pp_string fmt v.key;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "value" (Pbrt.Pp.pp_option pp_any_value) fmt v.value;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_instrumentation_scope fmt (v:instrumentation_scope) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.name;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "version" Pbrt.Pp.pp_string fmt v.version;
    if not (Pbrt.Bitfield.get v._presence 1) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
    if not (Pbrt.Bitfield.get v._presence 2) then Format.pp_print_string fmt "(* absent *)";
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_entity_ref fmt (v:entity_ref) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "type_" Pbrt.Pp.pp_string fmt v.type_;
    if not (Pbrt.Bitfield.get v._presence 1) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "id_keys" (Pbrt.Pp.pp_list Pbrt.Pp.pp_string) fmt v.id_keys;
    Pbrt.Pp.pp_record_field ~first:false "description_keys" (Pbrt.Pp.pp_list Pbrt.Pp.pp_string) fmt v.description_keys;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_any_value (v:any_value) encoder = 
  begin match v with
  | String_value x ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | Bool_value x ->
    Pbrt.Encoder.bool x encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  | Int_value x ->
    Pbrt.Encoder.int64_as_varint x encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  | Double_value x ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  | Array_value x ->
    Pbrt.Encoder.nested encode_pb_array_value x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  | Kvlist_value x ->
    Pbrt.Encoder.nested encode_pb_key_value_list x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  | Bytes_value x ->
    Pbrt.Encoder.bytes x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  end

and encode_pb_array_value (v:array_value) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_any_value x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.values encoder;
  ()

and encode_pb_key_value_list (v:key_value_list) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_key_value x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.values encoder;
  ()

and encode_pb_key_value (v:key_value) encoder = 
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.string v.key encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  begin match v.value with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_any_value x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_instrumentation_scope (v:instrumentation_scope) encoder = 
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 1) then (
    Pbrt.Encoder.string v.version encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_key_value x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  if (Pbrt.Bitfield.get v._presence 2) then (
    Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
    Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_entity_ref (v:entity_ref) encoder = 
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.string v.schema_url encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 1) then (
    Pbrt.Encoder.string v.type_ encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.id_keys encoder;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  ) v.description_keys encoder;
  ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_any_value d = 
  let rec loop () = 
    let ret:any_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "any_value"
      | Some (1, _) -> (String_value (Pbrt.Decoder.string d) : any_value) 
      | Some (2, _) -> (Bool_value (Pbrt.Decoder.bool d) : any_value) 
      | Some (3, _) -> (Int_value (Pbrt.Decoder.int64_as_varint d) : any_value) 
      | Some (4, _) -> (Double_value (Pbrt.Decoder.float_as_bits64 d) : any_value) 
      | Some (5, _) -> (Array_value (decode_pb_array_value (Pbrt.Decoder.nested d)) : any_value) 
      | Some (6, _) -> (Kvlist_value (decode_pb_key_value_list (Pbrt.Decoder.nested d)) : any_value) 
      | Some (7, _) -> (Bytes_value (Pbrt.Decoder.bytes d) : any_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_array_value d =
  let v = default_array_value () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_array_value_values v (List.rev v.values);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_array_value_values v ((decode_pb_any_value (Pbrt.Decoder.nested d)) :: v.values);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(array_value), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : array_value)

and decode_pb_key_value_list d =
  let v = default_key_value_list () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_key_value_list_values v (List.rev v.values);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_key_value_list_values v ((decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.values);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(key_value_list), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : key_value_list)

and decode_pb_key_value d =
  let v = default_key_value () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_key_value_key v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(key_value), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      set_key_value_value v (decode_pb_any_value (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(key_value), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : key_value)

let rec decode_pb_instrumentation_scope d =
  let v = default_instrumentation_scope () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_instrumentation_scope_attributes v (List.rev v.attributes);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_instrumentation_scope_name v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_scope), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      set_instrumentation_scope_version v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_scope), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      set_instrumentation_scope_attributes v ((decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_scope), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      set_instrumentation_scope_dropped_attributes_count v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_scope), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : instrumentation_scope)

let rec decode_pb_entity_ref d =
  let v = default_entity_ref () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_entity_ref_description_keys v (List.rev v.description_keys);
      set_entity_ref_id_keys v (List.rev v.id_keys);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_entity_ref_schema_url v (Pbrt.Decoder.string d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(entity_ref), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      set_entity_ref_type_ v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(entity_ref), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      set_entity_ref_id_keys v ((Pbrt.Decoder.string d) :: v.id_keys);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(entity_ref), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      set_entity_ref_description_keys v ((Pbrt.Decoder.string d) :: v.description_keys);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(entity_ref), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : entity_ref)
