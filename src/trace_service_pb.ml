[@@@ocaml.warning "-27-30-39"]

type export_trace_service_request_mutable = {
  mutable resource_spans : Trace_types.resource_spans list;
}

let default_export_trace_service_request_mutable () : export_trace_service_request_mutable = {
  resource_spans = [];
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

let rec encode_export_trace_service_request (v:Trace_service_types.export_trace_service_request) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Trace_pb.encode_resource_spans x) encoder;
  ) v.Trace_service_types.resource_spans;
  ()
