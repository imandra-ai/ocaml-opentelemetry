[@@@ocaml.warning "-27-30-39"]

type log_record_mutable = {
  mutable time_unix_nano : int64;
  mutable observed_time_unix_nano : int64;
  mutable severity_number : Logs_types.severity_number;
  mutable severity_text : string;
  mutable body : Common_types.any_value option;
  mutable attributes : Common_types.key_value list;
  mutable dropped_attributes_count : int32;
  mutable flags : int32;
  mutable trace_id : bytes;
  mutable span_id : bytes;
}

let default_log_record_mutable () : log_record_mutable = {
  time_unix_nano = 0L;
  observed_time_unix_nano = 0L;
  severity_number = Logs_types.default_severity_number ();
  severity_text = "";
  body = None;
  attributes = [];
  dropped_attributes_count = 0l;
  flags = 0l;
  trace_id = Bytes.create 0;
  span_id = Bytes.create 0;
}

type scope_logs_mutable = {
  mutable scope : Common_types.instrumentation_scope option;
  mutable log_records : Logs_types.log_record list;
  mutable schema_url : string;
}

let default_scope_logs_mutable () : scope_logs_mutable = {
  scope = None;
  log_records = [];
  schema_url = "";
}

type resource_logs_mutable = {
  mutable resource : Resource_types.resource option;
  mutable scope_logs : Logs_types.scope_logs list;
  mutable schema_url : string;
}

let default_resource_logs_mutable () : resource_logs_mutable = {
  resource = None;
  scope_logs = [];
  schema_url = "";
}

type logs_data_mutable = {
  mutable resource_logs : Logs_types.resource_logs list;
}

let default_logs_data_mutable () : logs_data_mutable = {
  resource_logs = [];
}


let rec decode_severity_number d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Logs_types.Severity_number_unspecified:Logs_types.severity_number)
  | 1 -> (Logs_types.Severity_number_trace:Logs_types.severity_number)
  | 2 -> (Logs_types.Severity_number_trace2:Logs_types.severity_number)
  | 3 -> (Logs_types.Severity_number_trace3:Logs_types.severity_number)
  | 4 -> (Logs_types.Severity_number_trace4:Logs_types.severity_number)
  | 5 -> (Logs_types.Severity_number_debug:Logs_types.severity_number)
  | 6 -> (Logs_types.Severity_number_debug2:Logs_types.severity_number)
  | 7 -> (Logs_types.Severity_number_debug3:Logs_types.severity_number)
  | 8 -> (Logs_types.Severity_number_debug4:Logs_types.severity_number)
  | 9 -> (Logs_types.Severity_number_info:Logs_types.severity_number)
  | 10 -> (Logs_types.Severity_number_info2:Logs_types.severity_number)
  | 11 -> (Logs_types.Severity_number_info3:Logs_types.severity_number)
  | 12 -> (Logs_types.Severity_number_info4:Logs_types.severity_number)
  | 13 -> (Logs_types.Severity_number_warn:Logs_types.severity_number)
  | 14 -> (Logs_types.Severity_number_warn2:Logs_types.severity_number)
  | 15 -> (Logs_types.Severity_number_warn3:Logs_types.severity_number)
  | 16 -> (Logs_types.Severity_number_warn4:Logs_types.severity_number)
  | 17 -> (Logs_types.Severity_number_error:Logs_types.severity_number)
  | 18 -> (Logs_types.Severity_number_error2:Logs_types.severity_number)
  | 19 -> (Logs_types.Severity_number_error3:Logs_types.severity_number)
  | 20 -> (Logs_types.Severity_number_error4:Logs_types.severity_number)
  | 21 -> (Logs_types.Severity_number_fatal:Logs_types.severity_number)
  | 22 -> (Logs_types.Severity_number_fatal2:Logs_types.severity_number)
  | 23 -> (Logs_types.Severity_number_fatal3:Logs_types.severity_number)
  | 24 -> (Logs_types.Severity_number_fatal4:Logs_types.severity_number)
  | _ -> Pbrt.Decoder.malformed_variant "severity_number"

let rec decode_log_record d =
  let v = default_log_record_mutable () in
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
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(1)" pk
    | Some (11, Pbrt.Bits64) -> begin
      v.observed_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(11)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.severity_number <- decode_severity_number d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.severity_text <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(3)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.body <- Some (Common_pb.decode_any_value (Pbrt.Decoder.nested d));
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.attributes <- (Common_pb.decode_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(6)" pk
    | Some (7, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(7)" pk
    | Some (8, Pbrt.Bits32) -> begin
      v.flags <- Pbrt.Decoder.int32_as_bits32 d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.trace_id <- Pbrt.Decoder.bytes d;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(9)" pk
    | Some (10, Pbrt.Bytes) -> begin
      v.span_id <- Pbrt.Decoder.bytes d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(10)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Logs_types.time_unix_nano = v.time_unix_nano;
    Logs_types.observed_time_unix_nano = v.observed_time_unix_nano;
    Logs_types.severity_number = v.severity_number;
    Logs_types.severity_text = v.severity_text;
    Logs_types.body = v.body;
    Logs_types.attributes = v.attributes;
    Logs_types.dropped_attributes_count = v.dropped_attributes_count;
    Logs_types.flags = v.flags;
    Logs_types.trace_id = v.trace_id;
    Logs_types.span_id = v.span_id;
  } : Logs_types.log_record)

let rec decode_scope_logs d =
  let v = default_scope_logs_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.log_records <- List.rev v.log_records;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.scope <- Some (Common_pb.decode_instrumentation_scope (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_logs), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.log_records <- (decode_log_record (Pbrt.Decoder.nested d)) :: v.log_records;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_logs), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_logs), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Logs_types.scope = v.scope;
    Logs_types.log_records = v.log_records;
    Logs_types.schema_url = v.schema_url;
  } : Logs_types.scope_logs)

let rec decode_resource_logs d =
  let v = default_resource_logs_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.scope_logs <- List.rev v.scope_logs;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource <- Some (Resource_pb.decode_resource (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_logs), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.scope_logs <- (decode_scope_logs (Pbrt.Decoder.nested d)) :: v.scope_logs;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_logs), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_logs), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Logs_types.resource = v.resource;
    Logs_types.scope_logs = v.scope_logs;
    Logs_types.schema_url = v.schema_url;
  } : Logs_types.resource_logs)

let rec decode_logs_data d =
  let v = default_logs_data_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_logs <- List.rev v.resource_logs;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_logs <- (decode_resource_logs (Pbrt.Decoder.nested d)) :: v.resource_logs;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(logs_data), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Logs_types.resource_logs = v.resource_logs;
  } : Logs_types.logs_data)

let rec decode_log_record_flags d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Logs_types.Log_record_flags_do_not_use:Logs_types.log_record_flags)
  | 255 -> (Logs_types.Log_record_flags_trace_flags_mask:Logs_types.log_record_flags)
  | _ -> Pbrt.Decoder.malformed_variant "log_record_flags"

let rec encode_severity_number (v:Logs_types.severity_number) encoder =
  match v with
  | Logs_types.Severity_number_unspecified -> Pbrt.Encoder.int_as_varint (0) encoder
  | Logs_types.Severity_number_trace -> Pbrt.Encoder.int_as_varint 1 encoder
  | Logs_types.Severity_number_trace2 -> Pbrt.Encoder.int_as_varint 2 encoder
  | Logs_types.Severity_number_trace3 -> Pbrt.Encoder.int_as_varint 3 encoder
  | Logs_types.Severity_number_trace4 -> Pbrt.Encoder.int_as_varint 4 encoder
  | Logs_types.Severity_number_debug -> Pbrt.Encoder.int_as_varint 5 encoder
  | Logs_types.Severity_number_debug2 -> Pbrt.Encoder.int_as_varint 6 encoder
  | Logs_types.Severity_number_debug3 -> Pbrt.Encoder.int_as_varint 7 encoder
  | Logs_types.Severity_number_debug4 -> Pbrt.Encoder.int_as_varint 8 encoder
  | Logs_types.Severity_number_info -> Pbrt.Encoder.int_as_varint 9 encoder
  | Logs_types.Severity_number_info2 -> Pbrt.Encoder.int_as_varint 10 encoder
  | Logs_types.Severity_number_info3 -> Pbrt.Encoder.int_as_varint 11 encoder
  | Logs_types.Severity_number_info4 -> Pbrt.Encoder.int_as_varint 12 encoder
  | Logs_types.Severity_number_warn -> Pbrt.Encoder.int_as_varint 13 encoder
  | Logs_types.Severity_number_warn2 -> Pbrt.Encoder.int_as_varint 14 encoder
  | Logs_types.Severity_number_warn3 -> Pbrt.Encoder.int_as_varint 15 encoder
  | Logs_types.Severity_number_warn4 -> Pbrt.Encoder.int_as_varint 16 encoder
  | Logs_types.Severity_number_error -> Pbrt.Encoder.int_as_varint 17 encoder
  | Logs_types.Severity_number_error2 -> Pbrt.Encoder.int_as_varint 18 encoder
  | Logs_types.Severity_number_error3 -> Pbrt.Encoder.int_as_varint 19 encoder
  | Logs_types.Severity_number_error4 -> Pbrt.Encoder.int_as_varint 20 encoder
  | Logs_types.Severity_number_fatal -> Pbrt.Encoder.int_as_varint 21 encoder
  | Logs_types.Severity_number_fatal2 -> Pbrt.Encoder.int_as_varint 22 encoder
  | Logs_types.Severity_number_fatal3 -> Pbrt.Encoder.int_as_varint 23 encoder
  | Logs_types.Severity_number_fatal4 -> Pbrt.Encoder.int_as_varint 24 encoder

let rec encode_log_record (v:Logs_types.log_record) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Logs_types.time_unix_nano encoder;
  Pbrt.Encoder.key (11, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.int64_as_bits64 v.Logs_types.observed_time_unix_nano encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  encode_severity_number v.Logs_types.severity_number encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Logs_types.severity_text encoder;
  begin match v.Logs_types.body with
  | Some x -> 
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_any_value x) encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_key_value x) encoder;
  ) v.Logs_types.attributes;
  Pbrt.Encoder.key (7, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Logs_types.dropped_attributes_count encoder;
  Pbrt.Encoder.key (8, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.int32_as_bits32 v.Logs_types.flags encoder;
  Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Logs_types.trace_id encoder;
  Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Logs_types.span_id encoder;
  ()

let rec encode_scope_logs (v:Logs_types.scope_logs) encoder = 
  begin match v.Logs_types.scope with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Common_pb.encode_instrumentation_scope x) encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_record x) encoder;
  ) v.Logs_types.log_records;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Logs_types.schema_url encoder;
  ()

let rec encode_resource_logs (v:Logs_types.resource_logs) encoder = 
  begin match v.Logs_types.resource with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Resource_pb.encode_resource x) encoder;
  | None -> ();
  end;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_scope_logs x) encoder;
  ) v.Logs_types.scope_logs;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Logs_types.schema_url encoder;
  ()

let rec encode_logs_data (v:Logs_types.logs_data) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_resource_logs x) encoder;
  ) v.Logs_types.resource_logs;
  ()

let rec encode_log_record_flags (v:Logs_types.log_record_flags) encoder =
  match v with
  | Logs_types.Log_record_flags_do_not_use -> Pbrt.Encoder.int_as_varint (0) encoder
  | Logs_types.Log_record_flags_trace_flags_mask -> Pbrt.Encoder.int_as_varint 255 encoder
