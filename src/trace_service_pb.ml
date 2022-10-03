[@@@ocaml.warning "-27-30-39"]

type export_trace_service_request_mutable = {
  mutable resource_spans : Trace_types.resource_spans list;
}

let default_export_trace_service_request_mutable () : export_trace_service_request_mutable = {
  resource_spans = [];
}

type export_trace_partial_success_mutable = {
  mutable rejected_spans : int64;
  mutable error_message : string;
}

let default_export_trace_partial_success_mutable () : export_trace_partial_success_mutable = {
  rejected_spans = 0L;
  error_message = "";
}

type export_trace_service_response_mutable = {
  mutable partial_success : Trace_service_types.export_trace_partial_success option;
}

let default_export_trace_service_response_mutable () : export_trace_service_response_mutable = {
  partial_success = None;
}


let rec decode_export_trace_service_request d =
  let v = default_export_trace_service_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_spans <- List.rev v.resource_spans;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_spans <- (Trace_pb.decode_resource_spans (Pbrt.Decoder.nested d)) :: v.resource_spans;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_trace_service_request), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Trace_service_types.resource_spans = v.resource_spans;
  } : Trace_service_types.export_trace_service_request)

let rec decode_export_trace_partial_success d =
  let v = default_export_trace_partial_success_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.rejected_spans <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_trace_partial_success), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.error_message <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_trace_partial_success), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Trace_service_types.rejected_spans = v.rejected_spans;
    Trace_service_types.error_message = v.error_message;
  } : Trace_service_types.export_trace_partial_success)

let rec decode_export_trace_service_response d =
  let v = default_export_trace_service_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.partial_success <- Some (decode_export_trace_partial_success (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_trace_service_response), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Trace_service_types.partial_success = v.partial_success;
  } : Trace_service_types.export_trace_service_response)

let rec encode_export_trace_service_request (v:Trace_service_types.export_trace_service_request) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Trace_pb.encode_resource_spans x) encoder;
  ) v.Trace_service_types.resource_spans;
  ()

let rec encode_export_trace_partial_success (v:Trace_service_types.export_trace_partial_success) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Trace_service_types.rejected_spans encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Trace_service_types.error_message encoder;
  ()

let rec encode_export_trace_service_response (v:Trace_service_types.export_trace_service_response) encoder = 
  begin match v.Trace_service_types.partial_success with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_export_trace_partial_success x) encoder;
  | None -> ();
  end;
  ()
