[@@@ocaml.warning "-23-27-30-39-44"]

type severity_number =
  | Severity_number_unspecified 
  | Severity_number_trace 
  | Severity_number_trace2 
  | Severity_number_trace3 
  | Severity_number_trace4 
  | Severity_number_debug 
  | Severity_number_debug2 
  | Severity_number_debug3 
  | Severity_number_debug4 
  | Severity_number_info 
  | Severity_number_info2 
  | Severity_number_info3 
  | Severity_number_info4 
  | Severity_number_warn 
  | Severity_number_warn2 
  | Severity_number_warn3 
  | Severity_number_warn4 
  | Severity_number_error 
  | Severity_number_error2 
  | Severity_number_error3 
  | Severity_number_error4 
  | Severity_number_fatal 
  | Severity_number_fatal2 
  | Severity_number_fatal3 
  | Severity_number_fatal4 

type log_record = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 9 fields *)
  mutable time_unix_nano : int64;
  mutable observed_time_unix_nano : int64;
  mutable severity_number : severity_number;
  mutable severity_text : string;
  mutable body : Common.any_value option;
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
  mutable flags : int32;
  mutable trace_id : bytes;
  mutable span_id : bytes;
  mutable event_name : string;
}

type scope_logs = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable scope : Common.instrumentation_scope option;
  mutable log_records : log_record list;
  mutable schema_url : string;
}

type resource_logs = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable resource : Resource.resource option;
  mutable scope_logs : scope_logs list;
  mutable schema_url : string;
}

type logs_data = {
  mutable resource_logs : resource_logs list;
}

type log_record_flags =
  | Log_record_flags_do_not_use 
  | Log_record_flags_trace_flags_mask 

let default_severity_number () = (Severity_number_unspecified:severity_number)

let default_log_record (): log_record =
{
  _presence=Pbrt.Bitfield.empty;
  time_unix_nano=0L;
  observed_time_unix_nano=0L;
  severity_number=default_severity_number ();
  severity_text="";
  body=None;
  attributes=[];
  dropped_attributes_count=0l;
  flags=0l;
  trace_id=Bytes.create 0;
  span_id=Bytes.create 0;
  event_name="";
}

let default_scope_logs (): scope_logs =
{
  _presence=Pbrt.Bitfield.empty;
  scope=None;
  log_records=[];
  schema_url="";
}

let default_resource_logs (): resource_logs =
{
  _presence=Pbrt.Bitfield.empty;
  resource=None;
  scope_logs=[];
  schema_url="";
}

let default_logs_data (): logs_data =
{
  resource_logs=[];
}

let default_log_record_flags () = (Log_record_flags_do_not_use:log_record_flags)


(** {2 Make functions} *)


let[@inline] log_record_has_time_unix_nano (self:log_record) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] log_record_has_observed_time_unix_nano (self:log_record) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] log_record_has_severity_number (self:log_record) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] log_record_has_severity_text (self:log_record) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] log_record_has_dropped_attributes_count (self:log_record) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] log_record_has_flags (self:log_record) : bool = (Pbrt.Bitfield.get self._presence 5)
let[@inline] log_record_has_trace_id (self:log_record) : bool = (Pbrt.Bitfield.get self._presence 6)
let[@inline] log_record_has_span_id (self:log_record) : bool = (Pbrt.Bitfield.get self._presence 7)
let[@inline] log_record_has_event_name (self:log_record) : bool = (Pbrt.Bitfield.get self._presence 8)

let[@inline] log_record_set_time_unix_nano (self:log_record) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.time_unix_nano <- x
let[@inline] log_record_set_observed_time_unix_nano (self:log_record) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.observed_time_unix_nano <- x
let[@inline] log_record_set_severity_number (self:log_record) (x:severity_number) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.severity_number <- x
let[@inline] log_record_set_severity_text (self:log_record) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.severity_text <- x
let[@inline] log_record_set_body (self:log_record) (x:Common.any_value) : unit =
  self.body <- Some x
let[@inline] log_record_set_attributes (self:log_record) (x:Common.key_value list) : unit =
  self.attributes <- x
let[@inline] log_record_set_dropped_attributes_count (self:log_record) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.dropped_attributes_count <- x
let[@inline] log_record_set_flags (self:log_record) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.flags <- x
let[@inline] log_record_set_trace_id (self:log_record) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 6); self.trace_id <- x
let[@inline] log_record_set_span_id (self:log_record) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 7); self.span_id <- x
let[@inline] log_record_set_event_name (self:log_record) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 8); self.event_name <- x

let copy_log_record (self:log_record) : log_record =
  { self with time_unix_nano = self.time_unix_nano }

let make_log_record 
  ?(time_unix_nano:int64 option)
  ?(observed_time_unix_nano:int64 option)
  ?(severity_number:severity_number option)
  ?(severity_text:string option)
  ?(body:Common.any_value option)
  ?(attributes=[])
  ?(dropped_attributes_count:int32 option)
  ?(flags:int32 option)
  ?(trace_id:bytes option)
  ?(span_id:bytes option)
  ?(event_name:string option)
  () : log_record  =
  let _res = default_log_record () in
  (match time_unix_nano with
  | None -> ()
  | Some v -> log_record_set_time_unix_nano _res v);
  (match observed_time_unix_nano with
  | None -> ()
  | Some v -> log_record_set_observed_time_unix_nano _res v);
  (match severity_number with
  | None -> ()
  | Some v -> log_record_set_severity_number _res v);
  (match severity_text with
  | None -> ()
  | Some v -> log_record_set_severity_text _res v);
  (match body with
  | None -> ()
  | Some v -> log_record_set_body _res v);
  log_record_set_attributes _res attributes;
  (match dropped_attributes_count with
  | None -> ()
  | Some v -> log_record_set_dropped_attributes_count _res v);
  (match flags with
  | None -> ()
  | Some v -> log_record_set_flags _res v);
  (match trace_id with
  | None -> ()
  | Some v -> log_record_set_trace_id _res v);
  (match span_id with
  | None -> ()
  | Some v -> log_record_set_span_id _res v);
  (match event_name with
  | None -> ()
  | Some v -> log_record_set_event_name _res v);
  _res

let[@inline] scope_logs_has_schema_url (self:scope_logs) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] scope_logs_set_scope (self:scope_logs) (x:Common.instrumentation_scope) : unit =
  self.scope <- Some x
let[@inline] scope_logs_set_log_records (self:scope_logs) (x:log_record list) : unit =
  self.log_records <- x
let[@inline] scope_logs_set_schema_url (self:scope_logs) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.schema_url <- x

let copy_scope_logs (self:scope_logs) : scope_logs =
  { self with scope = self.scope }

let make_scope_logs 
  ?(scope:Common.instrumentation_scope option)
  ?(log_records=[])
  ?(schema_url:string option)
  () : scope_logs  =
  let _res = default_scope_logs () in
  (match scope with
  | None -> ()
  | Some v -> scope_logs_set_scope _res v);
  scope_logs_set_log_records _res log_records;
  (match schema_url with
  | None -> ()
  | Some v -> scope_logs_set_schema_url _res v);
  _res

let[@inline] resource_logs_has_schema_url (self:resource_logs) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] resource_logs_set_resource (self:resource_logs) (x:Resource.resource) : unit =
  self.resource <- Some x
let[@inline] resource_logs_set_scope_logs (self:resource_logs) (x:scope_logs list) : unit =
  self.scope_logs <- x
let[@inline] resource_logs_set_schema_url (self:resource_logs) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.schema_url <- x

let copy_resource_logs (self:resource_logs) : resource_logs =
  { self with resource = self.resource }

let make_resource_logs 
  ?(resource:Resource.resource option)
  ?(scope_logs=[])
  ?(schema_url:string option)
  () : resource_logs  =
  let _res = default_resource_logs () in
  (match resource with
  | None -> ()
  | Some v -> resource_logs_set_resource _res v);
  resource_logs_set_scope_logs _res scope_logs;
  (match schema_url with
  | None -> ()
  | Some v -> resource_logs_set_schema_url _res v);
  _res


let[@inline] logs_data_set_resource_logs (self:logs_data) (x:resource_logs list) : unit =
  self.resource_logs <- x

let copy_logs_data (self:logs_data) : logs_data =
  { self with resource_logs = self.resource_logs }

let make_logs_data 
  ?(resource_logs=[])
  () : logs_data  =
  let _res = default_logs_data () in
  logs_data_set_resource_logs _res resource_logs;
  _res


[@@@ocaml.warning "-23-27-30-39"]

(** {2 Formatters} *)

let rec pp_severity_number fmt (v:severity_number) =
  match v with
  | Severity_number_unspecified -> Format.fprintf fmt "Severity_number_unspecified"
  | Severity_number_trace -> Format.fprintf fmt "Severity_number_trace"
  | Severity_number_trace2 -> Format.fprintf fmt "Severity_number_trace2"
  | Severity_number_trace3 -> Format.fprintf fmt "Severity_number_trace3"
  | Severity_number_trace4 -> Format.fprintf fmt "Severity_number_trace4"
  | Severity_number_debug -> Format.fprintf fmt "Severity_number_debug"
  | Severity_number_debug2 -> Format.fprintf fmt "Severity_number_debug2"
  | Severity_number_debug3 -> Format.fprintf fmt "Severity_number_debug3"
  | Severity_number_debug4 -> Format.fprintf fmt "Severity_number_debug4"
  | Severity_number_info -> Format.fprintf fmt "Severity_number_info"
  | Severity_number_info2 -> Format.fprintf fmt "Severity_number_info2"
  | Severity_number_info3 -> Format.fprintf fmt "Severity_number_info3"
  | Severity_number_info4 -> Format.fprintf fmt "Severity_number_info4"
  | Severity_number_warn -> Format.fprintf fmt "Severity_number_warn"
  | Severity_number_warn2 -> Format.fprintf fmt "Severity_number_warn2"
  | Severity_number_warn3 -> Format.fprintf fmt "Severity_number_warn3"
  | Severity_number_warn4 -> Format.fprintf fmt "Severity_number_warn4"
  | Severity_number_error -> Format.fprintf fmt "Severity_number_error"
  | Severity_number_error2 -> Format.fprintf fmt "Severity_number_error2"
  | Severity_number_error3 -> Format.fprintf fmt "Severity_number_error3"
  | Severity_number_error4 -> Format.fprintf fmt "Severity_number_error4"
  | Severity_number_fatal -> Format.fprintf fmt "Severity_number_fatal"
  | Severity_number_fatal2 -> Format.fprintf fmt "Severity_number_fatal2"
  | Severity_number_fatal3 -> Format.fprintf fmt "Severity_number_fatal3"
  | Severity_number_fatal4 -> Format.fprintf fmt "Severity_number_fatal4"

let rec pp_log_record fmt (v:log_record) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    if not (log_record_has_time_unix_nano v) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "observed_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.observed_time_unix_nano;
    if not (log_record_has_observed_time_unix_nano v) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "severity_number" pp_severity_number fmt v.severity_number;
    if not (log_record_has_severity_number v) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "severity_text" Pbrt.Pp.pp_string fmt v.severity_text;
    if not (log_record_has_severity_text v) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "body" (Pbrt.Pp.pp_option Common.pp_any_value) fmt v.body;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
    if not (log_record_has_dropped_attributes_count v) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
    if not (log_record_has_flags v) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "trace_id" Pbrt.Pp.pp_bytes fmt v.trace_id;
    if not (log_record_has_trace_id v) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.span_id;
    if not (log_record_has_span_id v) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "event_name" Pbrt.Pp.pp_string fmt v.event_name;
    if not (log_record_has_event_name v) then Format.pp_print_string fmt "(* absent *)";
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_scope_logs fmt (v:scope_logs) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "scope" (Pbrt.Pp.pp_option Common.pp_instrumentation_scope) fmt v.scope;
    Pbrt.Pp.pp_record_field ~first:false "log_records" (Pbrt.Pp.pp_list pp_log_record) fmt v.log_records;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
    if not (scope_logs_has_schema_url v) then Format.pp_print_string fmt "(* absent *)";
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_resource_logs fmt (v:resource_logs) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource" (Pbrt.Pp.pp_option Resource.pp_resource) fmt v.resource;
    Pbrt.Pp.pp_record_field ~first:false "scope_logs" (Pbrt.Pp.pp_list pp_scope_logs) fmt v.scope_logs;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
    if not (resource_logs_has_schema_url v) then Format.pp_print_string fmt "(* absent *)";
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_logs_data fmt (v:logs_data) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_logs" (Pbrt.Pp.pp_list pp_resource_logs) fmt v.resource_logs;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_log_record_flags fmt (v:log_record_flags) =
  match v with
  | Log_record_flags_do_not_use -> Format.fprintf fmt "Log_record_flags_do_not_use"
  | Log_record_flags_trace_flags_mask -> Format.fprintf fmt "Log_record_flags_trace_flags_mask"

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_severity_number (v:severity_number) encoder =
  match v with
  | Severity_number_unspecified -> Pbrt.Encoder.int_as_varint (0) encoder
  | Severity_number_trace -> Pbrt.Encoder.int_as_varint 1 encoder
  | Severity_number_trace2 -> Pbrt.Encoder.int_as_varint 2 encoder
  | Severity_number_trace3 -> Pbrt.Encoder.int_as_varint 3 encoder
  | Severity_number_trace4 -> Pbrt.Encoder.int_as_varint 4 encoder
  | Severity_number_debug -> Pbrt.Encoder.int_as_varint 5 encoder
  | Severity_number_debug2 -> Pbrt.Encoder.int_as_varint 6 encoder
  | Severity_number_debug3 -> Pbrt.Encoder.int_as_varint 7 encoder
  | Severity_number_debug4 -> Pbrt.Encoder.int_as_varint 8 encoder
  | Severity_number_info -> Pbrt.Encoder.int_as_varint 9 encoder
  | Severity_number_info2 -> Pbrt.Encoder.int_as_varint 10 encoder
  | Severity_number_info3 -> Pbrt.Encoder.int_as_varint 11 encoder
  | Severity_number_info4 -> Pbrt.Encoder.int_as_varint 12 encoder
  | Severity_number_warn -> Pbrt.Encoder.int_as_varint 13 encoder
  | Severity_number_warn2 -> Pbrt.Encoder.int_as_varint 14 encoder
  | Severity_number_warn3 -> Pbrt.Encoder.int_as_varint 15 encoder
  | Severity_number_warn4 -> Pbrt.Encoder.int_as_varint 16 encoder
  | Severity_number_error -> Pbrt.Encoder.int_as_varint 17 encoder
  | Severity_number_error2 -> Pbrt.Encoder.int_as_varint 18 encoder
  | Severity_number_error3 -> Pbrt.Encoder.int_as_varint 19 encoder
  | Severity_number_error4 -> Pbrt.Encoder.int_as_varint 20 encoder
  | Severity_number_fatal -> Pbrt.Encoder.int_as_varint 21 encoder
  | Severity_number_fatal2 -> Pbrt.Encoder.int_as_varint 22 encoder
  | Severity_number_fatal3 -> Pbrt.Encoder.int_as_varint 23 encoder
  | Severity_number_fatal4 -> Pbrt.Encoder.int_as_varint 24 encoder

let rec encode_pb_log_record (v:log_record) encoder = 
  if log_record_has_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
    Pbrt.Encoder.key 1 Pbrt.Bits64 encoder; 
  );
  if log_record_has_observed_time_unix_nano v then (
    Pbrt.Encoder.int64_as_bits64 v.observed_time_unix_nano encoder;
    Pbrt.Encoder.key 11 Pbrt.Bits64 encoder; 
  );
  if log_record_has_severity_number v then (
    encode_pb_severity_number v.severity_number encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  );
  if log_record_has_severity_text v then (
    Pbrt.Encoder.string v.severity_text encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  begin match v.body with
  | Some x -> 
    Pbrt.Encoder.nested Common.encode_pb_any_value x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  if log_record_has_dropped_attributes_count v then (
    Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
    Pbrt.Encoder.key 7 Pbrt.Varint encoder; 
  );
  if log_record_has_flags v then (
    Pbrt.Encoder.int32_as_bits32 v.flags encoder;
    Pbrt.Encoder.key 8 Pbrt.Bits32 encoder; 
  );
  if log_record_has_trace_id v then (
    Pbrt.Encoder.bytes v.trace_id encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  );
  if log_record_has_span_id v then (
    Pbrt.Encoder.bytes v.span_id encoder;
    Pbrt.Encoder.key 10 Pbrt.Bytes encoder; 
  );
  if log_record_has_event_name v then (
    Pbrt.Encoder.string v.event_name encoder;
    Pbrt.Encoder.key 12 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_scope_logs (v:scope_logs) encoder = 
  begin match v.scope with
  | Some x -> 
    Pbrt.Encoder.nested Common.encode_pb_instrumentation_scope x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_log_record x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.log_records encoder;
  if scope_logs_has_schema_url v then (
    Pbrt.Encoder.string v.schema_url encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_resource_logs (v:resource_logs) encoder = 
  begin match v.resource with
  | Some x -> 
    Pbrt.Encoder.nested Resource.encode_pb_resource x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_scope_logs x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.scope_logs encoder;
  if resource_logs_has_schema_url v then (
    Pbrt.Encoder.string v.schema_url encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_logs_data (v:logs_data) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_resource_logs x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.resource_logs encoder;
  ()

let rec encode_pb_log_record_flags (v:log_record_flags) encoder =
  match v with
  | Log_record_flags_do_not_use -> Pbrt.Encoder.int_as_varint (0) encoder
  | Log_record_flags_trace_flags_mask -> Pbrt.Encoder.int_as_varint 255 encoder

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_severity_number d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Severity_number_unspecified:severity_number)
  | 1 -> (Severity_number_trace:severity_number)
  | 2 -> (Severity_number_trace2:severity_number)
  | 3 -> (Severity_number_trace3:severity_number)
  | 4 -> (Severity_number_trace4:severity_number)
  | 5 -> (Severity_number_debug:severity_number)
  | 6 -> (Severity_number_debug2:severity_number)
  | 7 -> (Severity_number_debug3:severity_number)
  | 8 -> (Severity_number_debug4:severity_number)
  | 9 -> (Severity_number_info:severity_number)
  | 10 -> (Severity_number_info2:severity_number)
  | 11 -> (Severity_number_info3:severity_number)
  | 12 -> (Severity_number_info4:severity_number)
  | 13 -> (Severity_number_warn:severity_number)
  | 14 -> (Severity_number_warn2:severity_number)
  | 15 -> (Severity_number_warn3:severity_number)
  | 16 -> (Severity_number_warn4:severity_number)
  | 17 -> (Severity_number_error:severity_number)
  | 18 -> (Severity_number_error2:severity_number)
  | 19 -> (Severity_number_error3:severity_number)
  | 20 -> (Severity_number_error4:severity_number)
  | 21 -> (Severity_number_fatal:severity_number)
  | 22 -> (Severity_number_fatal2:severity_number)
  | 23 -> (Severity_number_fatal3:severity_number)
  | 24 -> (Severity_number_fatal4:severity_number)
  | _ -> Pbrt.Decoder.malformed_variant "severity_number"

let rec decode_pb_log_record d =
  let v = default_log_record () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      log_record_set_attributes v (List.rev v.attributes);
    ); continue__ := false
    | Some (1, Pbrt.Bits64) -> begin
      log_record_set_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(1)" pk
    | Some (11, Pbrt.Bits64) -> begin
      log_record_set_observed_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(11)" pk
    | Some (2, Pbrt.Varint) -> begin
      log_record_set_severity_number v (decode_pb_severity_number d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      log_record_set_severity_text v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(3)" pk
    | Some (5, Pbrt.Bytes) -> begin
      log_record_set_body v (Common.decode_pb_any_value (Pbrt.Decoder.nested d));
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      log_record_set_attributes v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(6)" pk
    | Some (7, Pbrt.Varint) -> begin
      log_record_set_dropped_attributes_count v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(7)" pk
    | Some (8, Pbrt.Bits32) -> begin
      log_record_set_flags v (Pbrt.Decoder.int32_as_bits32 d);
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      log_record_set_trace_id v (Pbrt.Decoder.bytes d);
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(9)" pk
    | Some (10, Pbrt.Bytes) -> begin
      log_record_set_span_id v (Pbrt.Decoder.bytes d);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(10)" pk
    | Some (12, Pbrt.Bytes) -> begin
      log_record_set_event_name v (Pbrt.Decoder.string d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(12)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : log_record)

let rec decode_pb_scope_logs d =
  let v = default_scope_logs () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      scope_logs_set_log_records v (List.rev v.log_records);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      scope_logs_set_scope v (Common.decode_pb_instrumentation_scope (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_logs), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      scope_logs_set_log_records v ((decode_pb_log_record (Pbrt.Decoder.nested d)) :: v.log_records);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_logs), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      scope_logs_set_schema_url v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_logs), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : scope_logs)

let rec decode_pb_resource_logs d =
  let v = default_resource_logs () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      resource_logs_set_scope_logs v (List.rev v.scope_logs);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      resource_logs_set_resource v (Resource.decode_pb_resource (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_logs), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      resource_logs_set_scope_logs v ((decode_pb_scope_logs (Pbrt.Decoder.nested d)) :: v.scope_logs);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_logs), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      resource_logs_set_schema_url v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_logs), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : resource_logs)

let rec decode_pb_logs_data d =
  let v = default_logs_data () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      logs_data_set_resource_logs v (List.rev v.resource_logs);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      logs_data_set_resource_logs v ((decode_pb_resource_logs (Pbrt.Decoder.nested d)) :: v.resource_logs);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(logs_data), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : logs_data)

let rec decode_pb_log_record_flags d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Log_record_flags_do_not_use:log_record_flags)
  | 255 -> (Log_record_flags_trace_flags_mask:log_record_flags)
  | _ -> Pbrt.Decoder.malformed_variant "log_record_flags"
