[@@@ocaml.warning "-23-27-30-39-44"]

type export_logs_service_request = {
  mutable resource_logs : Logs.resource_logs list;
}

type export_logs_partial_success = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable rejected_log_records : int64;
  mutable error_message : string;
}

type export_logs_service_response = {
  mutable partial_success : export_logs_partial_success option;
}

let default_export_logs_service_request (): export_logs_service_request =
{
  resource_logs=[];
}

let default_export_logs_partial_success (): export_logs_partial_success =
{
  _presence=Pbrt.Bitfield.empty;
  rejected_log_records=0L;
  error_message="";
}

let default_export_logs_service_response (): export_logs_service_response =
{
  partial_success=None;
}


(** {2 Make functions} *)


let[@inline] set_export_logs_service_request_resource_logs (self:export_logs_service_request) (x:Logs.resource_logs list) : unit =
  self.resource_logs <- x

let copy_export_logs_service_request (self:export_logs_service_request) : export_logs_service_request =
  { self with resource_logs = self.resource_logs }

let make_export_logs_service_request 
  ?(resource_logs=[])
  () : export_logs_service_request  =
  let _res = default_export_logs_service_request () in
  set_export_logs_service_request_resource_logs _res resource_logs;
  _res

let[@inline] has_export_logs_partial_success_rejected_log_records (self:export_logs_partial_success) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] has_export_logs_partial_success_error_message (self:export_logs_partial_success) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] set_export_logs_partial_success_rejected_log_records (self:export_logs_partial_success) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.rejected_log_records <- x
let[@inline] set_export_logs_partial_success_error_message (self:export_logs_partial_success) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.error_message <- x

let copy_export_logs_partial_success (self:export_logs_partial_success) : export_logs_partial_success =
  { self with rejected_log_records = self.rejected_log_records }

let make_export_logs_partial_success 
  ?(rejected_log_records:int64 option)
  ?(error_message:string option)
  () : export_logs_partial_success  =
  let _res = default_export_logs_partial_success () in
  (match rejected_log_records with
  | None -> ()
  | Some v -> set_export_logs_partial_success_rejected_log_records _res v);
  (match error_message with
  | None -> ()
  | Some v -> set_export_logs_partial_success_error_message _res v);
  _res


let[@inline] set_export_logs_service_response_partial_success (self:export_logs_service_response) (x:export_logs_partial_success) : unit =
  self.partial_success <- Some x

let copy_export_logs_service_response (self:export_logs_service_response) : export_logs_service_response =
  { self with partial_success = self.partial_success }

let make_export_logs_service_response 
  ?(partial_success:export_logs_partial_success option)
  () : export_logs_service_response  =
  let _res = default_export_logs_service_response () in
  (match partial_success with
  | None -> ()
  | Some v -> set_export_logs_service_response_partial_success _res v);
  _res

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Formatters} *)

let rec pp_export_logs_service_request fmt (v:export_logs_service_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_logs" (Pbrt.Pp.pp_list Logs.pp_resource_logs) fmt v.resource_logs;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_logs_partial_success fmt (v:export_logs_partial_success) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "rejected_log_records" Pbrt.Pp.pp_int64 fmt v.rejected_log_records;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "error_message" Pbrt.Pp.pp_string fmt v.error_message;
    if not (Pbrt.Bitfield.get v._presence 1) then Format.pp_print_string fmt "(* absent *)";
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_logs_service_response fmt (v:export_logs_service_response) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "partial_success" (Pbrt.Pp.pp_option pp_export_logs_partial_success) fmt v.partial_success;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_export_logs_service_request (v:export_logs_service_request) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Logs.encode_pb_resource_logs x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.resource_logs encoder;
  ()

let rec encode_pb_export_logs_partial_success (v:export_logs_partial_success) encoder = 
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.int64_as_varint v.rejected_log_records encoder;
    Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 1) then (
    Pbrt.Encoder.string v.error_message encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_export_logs_service_response (v:export_logs_service_response) encoder = 
  begin match v.partial_success with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_export_logs_partial_success x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_export_logs_service_request d =
  let v = default_export_logs_service_request () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_export_logs_service_request_resource_logs v (List.rev v.resource_logs);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_export_logs_service_request_resource_logs v ((Logs.decode_pb_resource_logs (Pbrt.Decoder.nested d)) :: v.resource_logs);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_service_request), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : export_logs_service_request)

let rec decode_pb_export_logs_partial_success d =
  let v = default_export_logs_partial_success () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      set_export_logs_partial_success_rejected_log_records v (Pbrt.Decoder.int64_as_varint d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_partial_success), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      set_export_logs_partial_success_error_message v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_partial_success), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : export_logs_partial_success)

let rec decode_pb_export_logs_service_response d =
  let v = default_export_logs_service_response () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_export_logs_service_response_partial_success v (decode_pb_export_logs_partial_success (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_service_response), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : export_logs_service_response)
