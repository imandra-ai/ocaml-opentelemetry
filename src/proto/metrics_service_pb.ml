[@@@ocaml.warning "-27-30-39"]

type export_metrics_service_request_mutable = {
  mutable resource_metrics : Metrics_types.resource_metrics list;
}

let default_export_metrics_service_request_mutable () : export_metrics_service_request_mutable = {
  resource_metrics = [];
}

type export_metrics_partial_success_mutable = {
  mutable rejected_data_points : int64;
  mutable error_message : string;
}

let default_export_metrics_partial_success_mutable () : export_metrics_partial_success_mutable = {
  rejected_data_points = 0L;
  error_message = "";
}

type export_metrics_service_response_mutable = {
  mutable partial_success : Metrics_service_types.export_metrics_partial_success option;
}

let default_export_metrics_service_response_mutable () : export_metrics_service_response_mutable = {
  partial_success = None;
}


let rec decode_export_metrics_service_request d =
  let v = default_export_metrics_service_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_metrics <- List.rev v.resource_metrics;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_metrics <- (Metrics_pb.decode_resource_metrics (Pbrt.Decoder.nested d)) :: v.resource_metrics;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_metrics_service_request), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_service_types.resource_metrics = v.resource_metrics;
  } : Metrics_service_types.export_metrics_service_request)

let rec decode_export_metrics_partial_success d =
  let v = default_export_metrics_partial_success_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.rejected_data_points <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_metrics_partial_success), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.error_message <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_metrics_partial_success), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_service_types.rejected_data_points = v.rejected_data_points;
    Metrics_service_types.error_message = v.error_message;
  } : Metrics_service_types.export_metrics_partial_success)

let rec decode_export_metrics_service_response d =
  let v = default_export_metrics_service_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.partial_success <- Some (decode_export_metrics_partial_success (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_metrics_service_response), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Metrics_service_types.partial_success = v.partial_success;
  } : Metrics_service_types.export_metrics_service_response)

let rec encode_export_metrics_service_request (v:Metrics_service_types.export_metrics_service_request) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Metrics_pb.encode_resource_metrics x) encoder;
  ) v.Metrics_service_types.resource_metrics;
  ()

let rec encode_export_metrics_partial_success (v:Metrics_service_types.export_metrics_partial_success) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Metrics_service_types.rejected_data_points encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Metrics_service_types.error_message encoder;
  ()

let rec encode_export_metrics_service_response (v:Metrics_service_types.export_metrics_service_response) encoder = 
  begin match v.Metrics_service_types.partial_success with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_export_metrics_partial_success x) encoder;
  | None -> ();
  end;
  ()
