[@@@ocaml.warning "-27-30-39"]

type span_event_mutable = {
  mutable time_unix_nano : int64;
  mutable name : string;
  mutable attributes : Common_types.key_value list;
  mutable dropped_attributes_count : int32;
}

let default_span_event_mutable () : span_event_mutable = {
  time_unix_nano = 0L;
  name = "";
  attributes = [];
  dropped_attributes_count = 0l;
}

type span_link_mutable = {
  mutable trace_id : bytes;
  mutable span_id : bytes;
  mutable trace_state : string;
  mutable attributes : Common_types.key_value list;
  mutable dropped_attributes_count : int32;
}

let default_span_link_mutable () : span_link_mutable = {
  trace_id = Bytes.create 0;
  span_id = Bytes.create 0;
  trace_state = "";
  attributes = [];
  dropped_attributes_count = 0l;
}

type status_mutable = {
  mutable message : string;
  mutable code : Trace_types.status_status_code;
}

let default_status_mutable () : status_mutable = {
  message = "";
  code = Trace_types.default_status_status_code ();
}

type span_mutable = {
  mutable trace_id : bytes;
  mutable span_id : bytes;
  mutable trace_state : string;
  mutable parent_span_id : bytes;
  mutable name : string;
  mutable kind : Trace_types.span_span_kind;
  mutable start_time_unix_nano : int64;
  mutable end_time_unix_nano : int64;
  mutable attributes : Common_types.key_value list;
  mutable dropped_attributes_count : int32;
  mutable events : Trace_types.span_event list;
  mutable dropped_events_count : int32;
  mutable links : Trace_types.span_link list;
  mutable dropped_links_count : int32;
  mutable status : Trace_types.status option;
}

let default_span_mutable () : span_mutable = {
  trace_id = Bytes.create 0;
  span_id = Bytes.create 0;
  trace_state = "";
  parent_span_id = Bytes.create 0;
  name = "";
  kind = Trace_types.default_span_span_kind ();
  start_time_unix_nano = 0L;
  end_time_unix_nano = 0L;
  attributes = [];
  dropped_attributes_count = 0l;
  events = [];
  dropped_events_count = 0l;
  links = [];
  dropped_links_count = 0l;
  status = None;
}

type instrumentation_library_spans_mutable = {
  mutable instrumentation_library : Common_types.instrumentation_library option;
  mutable spans : Trace_types.span list;
  mutable schema_url : string;
}

let default_instrumentation_library_spans_mutable () : instrumentation_library_spans_mutable = {
  instrumentation_library = None;
  spans = [];
  schema_url = "";
}

type resource_spans_mutable = {
  mutable resource : Resource_types.resource option;
  mutable instrumentation_library_spans : Trace_types.instrumentation_library_spans list;
  mutable schema_url : string;
}

let default_resource_spans_mutable () : resource_spans_mutable = {
  resource = None;
  instrumentation_library_spans = [];
  schema_url = "";
}

type traces_data_mutable = {
  mutable resource_spans : Trace_types.resource_spans list;
}

let default_traces_data_mutable () : traces_data_mutable = {
  resource_spans = [];
}


let rec decode_span_span_kind d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Trace_types.Span_kind_unspecified:Trace_types.span_span_kind)
  | 1 -> (Trace_types.Span_kind_internal:Trace_types.span_span_kind)
  | 2 -> (Trace_types.Span_kind_server:Trace_types.span_span_kind)
  | 3 -> (Trace_types.Span_kind_client:Trace_types.span_span_kind)
  | 4 -> (Trace_types.Span_kind_producer:Trace_types.span_span_kind)
  | 5 -> (Trace_types.Span_kind_consumer:Trace_types.span_span_kind)
  | _ -> Pbrt.Decoder.malformed_variant "span_span_kind"

let rec decode_span_event d =
  let v = default_span_event_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.attributes <- (Common_pb.decode_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Trace_types.time_unix_nano = v.time_unix_nano;
    Trace_types.name = v.name;
    Trace_types.attributes = v.attributes;
    Trace_types.dropped_attributes_count = v.dropped_attributes_count;
  } : Trace_types.span_event)

let rec decode_span_link d =
  let v = default_span_link_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.trace_id <- Pbrt.Decoder.bytes d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.span_id <- Pbrt.Decoder.bytes d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.trace_state <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.attributes <- (Common_pb.decode_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(4)" pk
    | Some (5, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Trace_types.trace_id = v.trace_id;
    Trace_types.span_id = v.span_id;
    Trace_types.trace_state = v.trace_state;
    Trace_types.attributes = v.attributes;
    Trace_types.dropped_attributes_count = v.dropped_attributes_count;
  } : Trace_types.span_link)

let rec decode_status_status_code d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Trace_types.Status_code_unset:Trace_types.status_status_code)
  | 1 -> (Trace_types.Status_code_ok:Trace_types.status_status_code)
  | 2 -> (Trace_types.Status_code_error:Trace_types.status_status_code)
  | _ -> Pbrt.Decoder.malformed_variant "status_status_code"

let rec decode_status d =
  let v = default_status_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (2, Pbrt.Bytes) -> begin
      v.message <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.code <- decode_status_status_code d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Trace_types.message = v.message;
    Trace_types.code = v.code;
  } : Trace_types.status)

let rec decode_span d =
  let v = default_span_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.links <- List.rev v.links;
      v.events <- List.rev v.events;
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.trace_id <- Pbrt.Decoder.bytes d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.span_id <- Pbrt.Decoder.bytes d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.trace_state <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.parent_span_id <- Pbrt.Decoder.bytes d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(5)" pk
    | Some (6, Pbrt.Varint) -> begin
      v.kind <- decode_span_span_kind d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(6)" pk
    | Some (7, Pbrt.Bits64) -> begin
      v.start_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(7)" pk
    | Some (8, Pbrt.Bits64) -> begin
      v.end_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.attributes <- (Common_pb.decode_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(9)" pk
    | Some (10, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.events <- (decode_span_event (Pbrt.Decoder.nested d)) :: v.events;
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(11)" pk
    | Some (12, Pbrt.Varint) -> begin
      v.dropped_events_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(12)" pk
    | Some (13, Pbrt.Bytes) -> begin
      v.links <- (decode_span_link (Pbrt.Decoder.nested d)) :: v.links;
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(13)" pk
    | Some (14, Pbrt.Varint) -> begin
      v.dropped_links_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(14)" pk
    | Some (15, Pbrt.Bytes) -> begin
      v.status <- Some (decode_status (Pbrt.Decoder.nested d));
    end
    | Some (15, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(15)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Trace_types.trace_id = v.trace_id;
    Trace_types.span_id = v.span_id;
    Trace_types.trace_state = v.trace_state;
    Trace_types.parent_span_id = v.parent_span_id;
    Trace_types.name = v.name;
    Trace_types.kind = v.kind;
    Trace_types.start_time_unix_nano = v.start_time_unix_nano;
    Trace_types.end_time_unix_nano = v.end_time_unix_nano;
    Trace_types.attributes = v.attributes;
    Trace_types.dropped_attributes_count = v.dropped_attributes_count;
    Trace_types.events = v.events;
    Trace_types.dropped_events_count = v.dropped_events_count;
    Trace_types.links = v.links;
    Trace_types.dropped_links_count = v.dropped_links_count;
    Trace_types.status = v.status;
  } : Trace_types.span)

let rec decode_instrumentation_library_spans d =
  let v = default_instrumentation_library_spans_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.spans <- List.rev v.spans;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.instrumentation_library <- Some (Common_pb.decode_instrumentation_library (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_library_spans), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.spans <- (decode_span (Pbrt.Decoder.nested d)) :: v.spans;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_library_spans), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_library_spans), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Trace_types.instrumentation_library = v.instrumentation_library;
    Trace_types.spans = v.spans;
    Trace_types.schema_url = v.schema_url;
  } : Trace_types.instrumentation_library_spans)

let rec decode_resource_spans d =
  let v = default_resource_spans_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.instrumentation_library_spans <- List.rev v.instrumentation_library_spans;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource <- Some (Resource_pb.decode_resource (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_spans), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.instrumentation_library_spans <- (decode_instrumentation_library_spans (Pbrt.Decoder.nested d)) :: v.instrumentation_library_spans;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_spans), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_spans), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Trace_types.resource = v.resource;
    Trace_types.instrumentation_library_spans = v.instrumentation_library_spans;
    Trace_types.schema_url = v.schema_url;
  } : Trace_types.resource_spans)

let rec decode_traces_data d =
  let v = default_traces_data_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_spans <- List.rev v.resource_spans;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_spans <- (decode_resource_spans (Pbrt.Decoder.nested d)) :: v.resource_spans;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(traces_data), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Trace_types.resource_spans = v.resource_spans;
  } : Trace_types.traces_data)

let rec encode_span_span_kind (v:Trace_types.span_span_kind) encoder =
  match v with
  | Trace_types.Span_kind_unspecified -> Pbrt.Encoder.int_as_varint (0) encoder
  | Trace_types.Span_kind_internal -> Pbrt.Encoder.int_as_varint 1 encoder
  | Trace_types.Span_kind_server -> Pbrt.Encoder.int_as_varint 2 encoder
  | Trace_types.Span_kind_client -> Pbrt.Encoder.int_as_varint 3 encoder
  | Trace_types.Span_kind_producer -> Pbrt.Encoder.int_as_varint 4 encoder
  | Trace_types.Span_kind_consumer -> Pbrt.Encoder.int_as_varint 5 encoder

let rec encode_span_event (v:Trace_types.span_event) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Trace_types.time_unix_nano encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Trace_types.name encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_key_value x) encoder;
  ) v.Trace_types.attributes;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Trace_types.dropped_attributes_count encoder;
  ()

let rec encode_span_link (v:Trace_types.span_link) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Trace_types.trace_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Trace_types.span_id encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Trace_types.trace_state encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_key_value x) encoder;
  ) v.Trace_types.attributes;
  Pbrt.Encoder.key (5, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Trace_types.dropped_attributes_count encoder;
  ()

let rec encode_status_status_code (v:Trace_types.status_status_code) encoder =
  match v with
  | Trace_types.Status_code_unset -> Pbrt.Encoder.int_as_varint (0) encoder
  | Trace_types.Status_code_ok -> Pbrt.Encoder.int_as_varint 1 encoder
  | Trace_types.Status_code_error -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_status (v:Trace_types.status) encoder = 
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Trace_types.message encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  encode_status_status_code v.Trace_types.code encoder;
  ()

let rec encode_span (v:Trace_types.span) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Trace_types.trace_id encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Trace_types.span_id encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Trace_types.trace_state encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Trace_types.parent_span_id encoder;
  Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Trace_types.name encoder;
  Pbrt.Encoder.key (6, Pbrt.Varint) encoder; 
  encode_span_span_kind v.Trace_types.kind encoder;
  Pbrt.Encoder.key (7, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Trace_types.start_time_unix_nano encoder;
  Pbrt.Encoder.key (8, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Trace_types.end_time_unix_nano encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_key_value x) encoder;
  ) v.Trace_types.attributes;
  Pbrt.Encoder.key (10, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Trace_types.dropped_attributes_count encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_span_event x) encoder;
  ) v.Trace_types.events;
  Pbrt.Encoder.key (12, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Trace_types.dropped_events_count encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (13, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_span_link x) encoder;
  ) v.Trace_types.links;
  Pbrt.Encoder.key (14, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Trace_types.dropped_links_count encoder;
  begin match v.Trace_types.status with
  | Some x -> 
    Pbrt.Encoder.key (15, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_status x) encoder;
  | None -> ();
  end;
  ()

let rec encode_instrumentation_library_spans (v:Trace_types.instrumentation_library_spans) encoder = 
  begin match v.Trace_types.instrumentation_library with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_instrumentation_library x) encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_span x) encoder;
  ) v.Trace_types.spans;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Trace_types.schema_url encoder;
  ()

let rec encode_resource_spans (v:Trace_types.resource_spans) encoder = 
  begin match v.Trace_types.resource with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Resource_pb.encode_resource x) encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_instrumentation_library_spans x) encoder;
  ) v.Trace_types.instrumentation_library_spans;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Trace_types.schema_url encoder;
  ()

let rec encode_traces_data (v:Trace_types.traces_data) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_resource_spans x) encoder;
  ) v.Trace_types.resource_spans;
  ()
