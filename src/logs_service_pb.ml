[@@@ocaml.warning "-27-30-39"]

type export_logs_service_request_mutable = {
  mutable resource_logs : Logs_types.resource_logs list;
}

let default_export_logs_service_request_mutable () : export_logs_service_request_mutable = {
  resource_logs = [];
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

let rec encode_export_logs_service_request (v:Logs_service_types.export_logs_service_request) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Logs_pb.encode_resource_logs x) encoder;
  ) v.Logs_service_types.resource_logs;
  ()
