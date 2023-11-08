[@@@ocaml.warning "-27-30-39"]

type export_logs_service_request_mutable = {
  mutable resource_logs : Logs_types.resource_logs list;
}

let default_export_logs_service_request_mutable () : export_logs_service_request_mutable = {
  resource_logs = [];
}

type export_logs_partial_success_mutable = {
  mutable rejected_log_records : int64;
  mutable error_message : string;
}

let default_export_logs_partial_success_mutable () : export_logs_partial_success_mutable = {
  rejected_log_records = 0L;
  error_message = "";
}

type export_logs_service_response_mutable = {
  mutable partial_success : Logs_service_types.export_logs_partial_success option;
}

let default_export_logs_service_response_mutable () : export_logs_service_response_mutable = {
  partial_success = None;
}


let rec decode_export_logs_service_request d =
  let v = default_export_logs_service_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_logs <- List.rev v.resource_logs;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_logs <- (Logs_pb.decode_resource_logs (Pbrt.Decoder.nested d)) :: v.resource_logs;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_service_request), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Logs_service_types.resource_logs = v.resource_logs;
  } : Logs_service_types.export_logs_service_request)

let rec decode_export_logs_partial_success d =
  let v = default_export_logs_partial_success_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.rejected_log_records <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_partial_success), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.error_message <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_partial_success), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Logs_service_types.rejected_log_records = v.rejected_log_records;
    Logs_service_types.error_message = v.error_message;
  } : Logs_service_types.export_logs_partial_success)

let rec decode_export_logs_service_response d =
  let v = default_export_logs_service_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.partial_success <- Some (decode_export_logs_partial_success (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_service_response), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Logs_service_types.partial_success = v.partial_success;
  } : Logs_service_types.export_logs_service_response)

let rec encode_export_logs_service_request (v:Logs_service_types.export_logs_service_request) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Logs_pb.encode_resource_logs x) encoder;
  ) v.Logs_service_types.resource_logs;
  ()

let rec encode_export_logs_partial_success (v:Logs_service_types.export_logs_partial_success) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Logs_service_types.rejected_log_records encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Logs_service_types.error_message encoder;
  ()

let rec encode_export_logs_service_response (v:Logs_service_types.export_logs_service_response) encoder = 
  begin match v.Logs_service_types.partial_success with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_export_logs_partial_success x) encoder;
  | None -> ();
  end;
  ()
