[@@@ocaml.warning "-23-27-30-39-44"]

type status = {
  mutable _presence: Pbrt.Bitfield.t;
  (** tracking presence for 2 fields *)
  mutable code : int32;
  mutable message : bytes;
  mutable details : bytes list;
}

let default_status (): status = 
{
  _presence=Pbrt.Bitfield.empty;
  code=0l;
  message=Bytes.create 0;
  details=[];
}


(** {2 Make functions} *)

let[@inline] has_status_code (self:status) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] has_status_message (self:status) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] set_status_code (self:status) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.code <- x
let[@inline] set_status_message (self:status) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.message <- x
let[@inline] set_status_details (self:status) (x:bytes list) : unit =
  self.details <- x

let copy_status (self:status) : status =
  { self with code = self.code }

let make_status 
  ?(code:int32 option)
  ?(message:bytes option)
  ~(details:bytes list) 
  () : status  =
  let _res = default_status () in
  (match code with
  | None -> ()
  | Some v -> set_status_code _res v);
  (match message with
  | None -> ()
  | Some v -> set_status_message _res v);
  set_status_details _res details;
  _res

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Formatters} *)

let rec pp_status fmt (v:status) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "code" Pbrt.Pp.pp_int32 fmt v.code;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "message" Pbrt.Pp.pp_bytes fmt v.message;
    if not (Pbrt.Bitfield.get v._presence 1) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "details" (Pbrt.Pp.pp_list Pbrt.Pp.pp_bytes) fmt v.details;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_status (v:status) encoder = 
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.int32_as_varint v.code encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 1) then (
    Pbrt.Encoder.bytes v.message encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.bytes x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.details encoder;
  ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_status d =
  let v = default_status () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_status_details v (List.rev v.details);
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      set_status_code v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      set_status_message v (Pbrt.Decoder.bytes d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      set_status_details v ((Pbrt.Decoder.bytes d) :: v.details);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : status)
