[@@@ocaml.warning "-27-30-39"]

type export_metrics_service_request_mutable = {
  mutable resource_metrics : Metrics_types.resource_metrics list;
}

let default_export_metrics_service_request_mutable () : export_metrics_service_request_mutable = {
  resource_metrics = [];
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

let rec encode_export_metrics_service_request (v:Metrics_service_types.export_metrics_service_request) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Metrics_pb.encode_resource_metrics x) encoder;
  ) v.Metrics_service_types.resource_metrics;
  ()
