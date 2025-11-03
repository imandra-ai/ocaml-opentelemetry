[@@@ocaml.warning "-23-27-30-39-44"]

type span_span_kind =
  | Span_kind_unspecified 
  | Span_kind_internal 
  | Span_kind_server 
  | Span_kind_client 
  | Span_kind_producer 
  | Span_kind_consumer 

type span_event = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable time_unix_nano : int64;
  mutable name : string;
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
}

type span_link = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 5 fields *)
  mutable trace_id : bytes;
  mutable span_id : bytes;
  mutable trace_state : string;
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
  mutable flags : int32;
}

type status_status_code =
  | Status_code_unset 
  | Status_code_ok 
  | Status_code_error 

type status = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable message : string;
  mutable code : status_status_code;
}

type span = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 12 fields *)
  mutable trace_id : bytes;
  mutable span_id : bytes;
  mutable trace_state : string;
  mutable parent_span_id : bytes;
  mutable flags : int32;
  mutable name : string;
  mutable kind : span_span_kind;
  mutable start_time_unix_nano : int64;
  mutable end_time_unix_nano : int64;
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
  mutable events : span_event list;
  mutable dropped_events_count : int32;
  mutable links : span_link list;
  mutable dropped_links_count : int32;
  mutable status : status option;
}

type scope_spans = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable scope : Common.instrumentation_scope option;
  mutable spans : span list;
  mutable schema_url : string;
}

type resource_spans = {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable resource : Resource.resource option;
  mutable scope_spans : scope_spans list;
  mutable schema_url : string;
}

type traces_data = {
  mutable resource_spans : resource_spans list;
}

type span_flags =
  | Span_flags_do_not_use 
  | Span_flags_trace_flags_mask 
  | Span_flags_context_has_is_remote_mask 
  | Span_flags_context_is_remote_mask 

let default_span_span_kind () = (Span_kind_unspecified:span_span_kind)

let default_span_event (): span_event =
{
  _presence=Pbrt.Bitfield.empty;
  time_unix_nano=0L;
  name="";
  attributes=[];
  dropped_attributes_count=0l;
}

let default_span_link (): span_link =
{
  _presence=Pbrt.Bitfield.empty;
  trace_id=Bytes.create 0;
  span_id=Bytes.create 0;
  trace_state="";
  attributes=[];
  dropped_attributes_count=0l;
  flags=0l;
}

let default_status_status_code () = (Status_code_unset:status_status_code)

let default_status (): status =
{
  _presence=Pbrt.Bitfield.empty;
  message="";
  code=default_status_status_code ();
}

let default_span (): span =
{
  _presence=Pbrt.Bitfield.empty;
  trace_id=Bytes.create 0;
  span_id=Bytes.create 0;
  trace_state="";
  parent_span_id=Bytes.create 0;
  flags=0l;
  name="";
  kind=default_span_span_kind ();
  start_time_unix_nano=0L;
  end_time_unix_nano=0L;
  attributes=[];
  dropped_attributes_count=0l;
  events=[];
  dropped_events_count=0l;
  links=[];
  dropped_links_count=0l;
  status=None;
}

let default_scope_spans (): scope_spans =
{
  _presence=Pbrt.Bitfield.empty;
  scope=None;
  spans=[];
  schema_url="";
}

let default_resource_spans (): resource_spans =
{
  _presence=Pbrt.Bitfield.empty;
  resource=None;
  scope_spans=[];
  schema_url="";
}

let default_traces_data (): traces_data =
{
  resource_spans=[];
}

let default_span_flags () = (Span_flags_do_not_use:span_flags)


(** {2 Make functions} *)


let[@inline] has_span_event_time_unix_nano (self:span_event) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] has_span_event_name (self:span_event) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] has_span_event_dropped_attributes_count (self:span_event) : bool = (Pbrt.Bitfield.get self._presence 2)

let[@inline] set_span_event_time_unix_nano (self:span_event) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.time_unix_nano <- x
let[@inline] set_span_event_name (self:span_event) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.name <- x
let[@inline] set_span_event_attributes (self:span_event) (x:Common.key_value list) : unit =
  self.attributes <- x
let[@inline] set_span_event_dropped_attributes_count (self:span_event) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.dropped_attributes_count <- x

let copy_span_event (self:span_event) : span_event =
  { self with time_unix_nano = self.time_unix_nano }

let make_span_event 
  ?(time_unix_nano:int64 option)
  ?(name:string option)
  ?(attributes=[])
  ?(dropped_attributes_count:int32 option)
  () : span_event  =
  let _res = default_span_event () in
  (match time_unix_nano with
  | None -> ()
  | Some v -> set_span_event_time_unix_nano _res v);
  (match name with
  | None -> ()
  | Some v -> set_span_event_name _res v);
  set_span_event_attributes _res attributes;
  (match dropped_attributes_count with
  | None -> ()
  | Some v -> set_span_event_dropped_attributes_count _res v);
  _res

let[@inline] has_span_link_trace_id (self:span_link) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] has_span_link_span_id (self:span_link) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] has_span_link_trace_state (self:span_link) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] has_span_link_dropped_attributes_count (self:span_link) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] has_span_link_flags (self:span_link) : bool = (Pbrt.Bitfield.get self._presence 4)

let[@inline] set_span_link_trace_id (self:span_link) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.trace_id <- x
let[@inline] set_span_link_span_id (self:span_link) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.span_id <- x
let[@inline] set_span_link_trace_state (self:span_link) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.trace_state <- x
let[@inline] set_span_link_attributes (self:span_link) (x:Common.key_value list) : unit =
  self.attributes <- x
let[@inline] set_span_link_dropped_attributes_count (self:span_link) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.dropped_attributes_count <- x
let[@inline] set_span_link_flags (self:span_link) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.flags <- x

let copy_span_link (self:span_link) : span_link =
  { self with trace_id = self.trace_id }

let make_span_link 
  ?(trace_id:bytes option)
  ?(span_id:bytes option)
  ?(trace_state:string option)
  ?(attributes=[])
  ?(dropped_attributes_count:int32 option)
  ?(flags:int32 option)
  () : span_link  =
  let _res = default_span_link () in
  (match trace_id with
  | None -> ()
  | Some v -> set_span_link_trace_id _res v);
  (match span_id with
  | None -> ()
  | Some v -> set_span_link_span_id _res v);
  (match trace_state with
  | None -> ()
  | Some v -> set_span_link_trace_state _res v);
  set_span_link_attributes _res attributes;
  (match dropped_attributes_count with
  | None -> ()
  | Some v -> set_span_link_dropped_attributes_count _res v);
  (match flags with
  | None -> ()
  | Some v -> set_span_link_flags _res v);
  _res


let[@inline] has_status_message (self:status) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] has_status_code (self:status) : bool = (Pbrt.Bitfield.get self._presence 1)

let[@inline] set_status_message (self:status) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.message <- x
let[@inline] set_status_code (self:status) (x:status_status_code) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.code <- x

let copy_status (self:status) : status =
  { self with message = self.message }

let make_status 
  ?(message:string option)
  ?(code:status_status_code option)
  () : status  =
  let _res = default_status () in
  (match message with
  | None -> ()
  | Some v -> set_status_message _res v);
  (match code with
  | None -> ()
  | Some v -> set_status_code _res v);
  _res

let[@inline] has_span_trace_id (self:span) : bool = (Pbrt.Bitfield.get self._presence 0)
let[@inline] has_span_span_id (self:span) : bool = (Pbrt.Bitfield.get self._presence 1)
let[@inline] has_span_trace_state (self:span) : bool = (Pbrt.Bitfield.get self._presence 2)
let[@inline] has_span_parent_span_id (self:span) : bool = (Pbrt.Bitfield.get self._presence 3)
let[@inline] has_span_flags (self:span) : bool = (Pbrt.Bitfield.get self._presence 4)
let[@inline] has_span_name (self:span) : bool = (Pbrt.Bitfield.get self._presence 5)
let[@inline] has_span_kind (self:span) : bool = (Pbrt.Bitfield.get self._presence 6)
let[@inline] has_span_start_time_unix_nano (self:span) : bool = (Pbrt.Bitfield.get self._presence 7)
let[@inline] has_span_end_time_unix_nano (self:span) : bool = (Pbrt.Bitfield.get self._presence 8)
let[@inline] has_span_dropped_attributes_count (self:span) : bool = (Pbrt.Bitfield.get self._presence 9)
let[@inline] has_span_dropped_events_count (self:span) : bool = (Pbrt.Bitfield.get self._presence 10)
let[@inline] has_span_dropped_links_count (self:span) : bool = (Pbrt.Bitfield.get self._presence 11)

let[@inline] set_span_trace_id (self:span) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.trace_id <- x
let[@inline] set_span_span_id (self:span) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 1); self.span_id <- x
let[@inline] set_span_trace_state (self:span) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 2); self.trace_state <- x
let[@inline] set_span_parent_span_id (self:span) (x:bytes) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 3); self.parent_span_id <- x
let[@inline] set_span_flags (self:span) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 4); self.flags <- x
let[@inline] set_span_name (self:span) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 5); self.name <- x
let[@inline] set_span_kind (self:span) (x:span_span_kind) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 6); self.kind <- x
let[@inline] set_span_start_time_unix_nano (self:span) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 7); self.start_time_unix_nano <- x
let[@inline] set_span_end_time_unix_nano (self:span) (x:int64) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 8); self.end_time_unix_nano <- x
let[@inline] set_span_attributes (self:span) (x:Common.key_value list) : unit =
  self.attributes <- x
let[@inline] set_span_dropped_attributes_count (self:span) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 9); self.dropped_attributes_count <- x
let[@inline] set_span_events (self:span) (x:span_event list) : unit =
  self.events <- x
let[@inline] set_span_dropped_events_count (self:span) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 10); self.dropped_events_count <- x
let[@inline] set_span_links (self:span) (x:span_link list) : unit =
  self.links <- x
let[@inline] set_span_dropped_links_count (self:span) (x:int32) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 11); self.dropped_links_count <- x
let[@inline] set_span_status (self:span) (x:status) : unit =
  self.status <- Some x

let copy_span (self:span) : span =
  { self with trace_id = self.trace_id }

let make_span 
  ?(trace_id:bytes option)
  ?(span_id:bytes option)
  ?(trace_state:string option)
  ?(parent_span_id:bytes option)
  ?(flags:int32 option)
  ?(name:string option)
  ?(kind:span_span_kind option)
  ?(start_time_unix_nano:int64 option)
  ?(end_time_unix_nano:int64 option)
  ?(attributes=[])
  ?(dropped_attributes_count:int32 option)
  ?(events=[])
  ?(dropped_events_count:int32 option)
  ?(links=[])
  ?(dropped_links_count:int32 option)
  ?(status:status option)
  () : span  =
  let _res = default_span () in
  (match trace_id with
  | None -> ()
  | Some v -> set_span_trace_id _res v);
  (match span_id with
  | None -> ()
  | Some v -> set_span_span_id _res v);
  (match trace_state with
  | None -> ()
  | Some v -> set_span_trace_state _res v);
  (match parent_span_id with
  | None -> ()
  | Some v -> set_span_parent_span_id _res v);
  (match flags with
  | None -> ()
  | Some v -> set_span_flags _res v);
  (match name with
  | None -> ()
  | Some v -> set_span_name _res v);
  (match kind with
  | None -> ()
  | Some v -> set_span_kind _res v);
  (match start_time_unix_nano with
  | None -> ()
  | Some v -> set_span_start_time_unix_nano _res v);
  (match end_time_unix_nano with
  | None -> ()
  | Some v -> set_span_end_time_unix_nano _res v);
  set_span_attributes _res attributes;
  (match dropped_attributes_count with
  | None -> ()
  | Some v -> set_span_dropped_attributes_count _res v);
  set_span_events _res events;
  (match dropped_events_count with
  | None -> ()
  | Some v -> set_span_dropped_events_count _res v);
  set_span_links _res links;
  (match dropped_links_count with
  | None -> ()
  | Some v -> set_span_dropped_links_count _res v);
  (match status with
  | None -> ()
  | Some v -> set_span_status _res v);
  _res

let[@inline] has_scope_spans_schema_url (self:scope_spans) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] set_scope_spans_scope (self:scope_spans) (x:Common.instrumentation_scope) : unit =
  self.scope <- Some x
let[@inline] set_scope_spans_spans (self:scope_spans) (x:span list) : unit =
  self.spans <- x
let[@inline] set_scope_spans_schema_url (self:scope_spans) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.schema_url <- x

let copy_scope_spans (self:scope_spans) : scope_spans =
  { self with scope = self.scope }

let make_scope_spans 
  ?(scope:Common.instrumentation_scope option)
  ?(spans=[])
  ?(schema_url:string option)
  () : scope_spans  =
  let _res = default_scope_spans () in
  (match scope with
  | None -> ()
  | Some v -> set_scope_spans_scope _res v);
  set_scope_spans_spans _res spans;
  (match schema_url with
  | None -> ()
  | Some v -> set_scope_spans_schema_url _res v);
  _res

let[@inline] has_resource_spans_schema_url (self:resource_spans) : bool = (Pbrt.Bitfield.get self._presence 0)

let[@inline] set_resource_spans_resource (self:resource_spans) (x:Resource.resource) : unit =
  self.resource <- Some x
let[@inline] set_resource_spans_scope_spans (self:resource_spans) (x:scope_spans list) : unit =
  self.scope_spans <- x
let[@inline] set_resource_spans_schema_url (self:resource_spans) (x:string) : unit =
  self._presence <- (Pbrt.Bitfield.set self._presence 0); self.schema_url <- x

let copy_resource_spans (self:resource_spans) : resource_spans =
  { self with resource = self.resource }

let make_resource_spans 
  ?(resource:Resource.resource option)
  ?(scope_spans=[])
  ?(schema_url:string option)
  () : resource_spans  =
  let _res = default_resource_spans () in
  (match resource with
  | None -> ()
  | Some v -> set_resource_spans_resource _res v);
  set_resource_spans_scope_spans _res scope_spans;
  (match schema_url with
  | None -> ()
  | Some v -> set_resource_spans_schema_url _res v);
  _res


let[@inline] set_traces_data_resource_spans (self:traces_data) (x:resource_spans list) : unit =
  self.resource_spans <- x

let copy_traces_data (self:traces_data) : traces_data =
  { self with resource_spans = self.resource_spans }

let make_traces_data 
  ?(resource_spans=[])
  () : traces_data  =
  let _res = default_traces_data () in
  set_traces_data_resource_spans _res resource_spans;
  _res


[@@@ocaml.warning "-23-27-30-39"]

(** {2 Formatters} *)

let rec pp_span_span_kind fmt (v:span_span_kind) =
  match v with
  | Span_kind_unspecified -> Format.fprintf fmt "Span_kind_unspecified"
  | Span_kind_internal -> Format.fprintf fmt "Span_kind_internal"
  | Span_kind_server -> Format.fprintf fmt "Span_kind_server"
  | Span_kind_client -> Format.fprintf fmt "Span_kind_client"
  | Span_kind_producer -> Format.fprintf fmt "Span_kind_producer"
  | Span_kind_consumer -> Format.fprintf fmt "Span_kind_consumer"

let rec pp_span_event fmt (v:span_event) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "name" Pbrt.Pp.pp_string fmt v.name;
    if not (Pbrt.Bitfield.get v._presence 1) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
    if not (Pbrt.Bitfield.get v._presence 2) then Format.pp_print_string fmt "(* absent *)";
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_span_link fmt (v:span_link) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "trace_id" Pbrt.Pp.pp_bytes fmt v.trace_id;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.span_id;
    if not (Pbrt.Bitfield.get v._presence 1) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "trace_state" Pbrt.Pp.pp_string fmt v.trace_state;
    if not (Pbrt.Bitfield.get v._presence 2) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
    if not (Pbrt.Bitfield.get v._presence 3) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
    if not (Pbrt.Bitfield.get v._presence 4) then Format.pp_print_string fmt "(* absent *)";
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_status_status_code fmt (v:status_status_code) =
  match v with
  | Status_code_unset -> Format.fprintf fmt "Status_code_unset"
  | Status_code_ok -> Format.fprintf fmt "Status_code_ok"
  | Status_code_error -> Format.fprintf fmt "Status_code_error"

let rec pp_status fmt (v:status) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "message" Pbrt.Pp.pp_string fmt v.message;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "code" pp_status_status_code fmt v.code;
    if not (Pbrt.Bitfield.get v._presence 1) then Format.pp_print_string fmt "(* absent *)";
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_span fmt (v:span) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "trace_id" Pbrt.Pp.pp_bytes fmt v.trace_id;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.span_id;
    if not (Pbrt.Bitfield.get v._presence 1) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "trace_state" Pbrt.Pp.pp_string fmt v.trace_state;
    if not (Pbrt.Bitfield.get v._presence 2) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "parent_span_id" Pbrt.Pp.pp_bytes fmt v.parent_span_id;
    if not (Pbrt.Bitfield.get v._presence 3) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
    if not (Pbrt.Bitfield.get v._presence 4) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "name" Pbrt.Pp.pp_string fmt v.name;
    if not (Pbrt.Bitfield.get v._presence 5) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "kind" pp_span_span_kind fmt v.kind;
    if not (Pbrt.Bitfield.get v._presence 6) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.start_time_unix_nano;
    if not (Pbrt.Bitfield.get v._presence 7) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "end_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.end_time_unix_nano;
    if not (Pbrt.Bitfield.get v._presence 8) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
    if not (Pbrt.Bitfield.get v._presence 9) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "events" (Pbrt.Pp.pp_list pp_span_event) fmt v.events;
    Pbrt.Pp.pp_record_field ~first:false "dropped_events_count" Pbrt.Pp.pp_int32 fmt v.dropped_events_count;
    if not (Pbrt.Bitfield.get v._presence 10) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "links" (Pbrt.Pp.pp_list pp_span_link) fmt v.links;
    Pbrt.Pp.pp_record_field ~first:false "dropped_links_count" Pbrt.Pp.pp_int32 fmt v.dropped_links_count;
    if not (Pbrt.Bitfield.get v._presence 11) then Format.pp_print_string fmt "(* absent *)";
    Pbrt.Pp.pp_record_field ~first:false "status" (Pbrt.Pp.pp_option pp_status) fmt v.status;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_scope_spans fmt (v:scope_spans) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "scope" (Pbrt.Pp.pp_option Common.pp_instrumentation_scope) fmt v.scope;
    Pbrt.Pp.pp_record_field ~first:false "spans" (Pbrt.Pp.pp_list pp_span) fmt v.spans;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_resource_spans fmt (v:resource_spans) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource" (Pbrt.Pp.pp_option Resource.pp_resource) fmt v.resource;
    Pbrt.Pp.pp_record_field ~first:false "scope_spans" (Pbrt.Pp.pp_list pp_scope_spans) fmt v.scope_spans;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
    if not (Pbrt.Bitfield.get v._presence 0) then Format.pp_print_string fmt "(* absent *)";
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_traces_data fmt (v:traces_data) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_spans" (Pbrt.Pp.pp_list pp_resource_spans) fmt v.resource_spans;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_span_flags fmt (v:span_flags) =
  match v with
  | Span_flags_do_not_use -> Format.fprintf fmt "Span_flags_do_not_use"
  | Span_flags_trace_flags_mask -> Format.fprintf fmt "Span_flags_trace_flags_mask"
  | Span_flags_context_has_is_remote_mask -> Format.fprintf fmt "Span_flags_context_has_is_remote_mask"
  | Span_flags_context_is_remote_mask -> Format.fprintf fmt "Span_flags_context_is_remote_mask"

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_span_span_kind (v:span_span_kind) encoder =
  match v with
  | Span_kind_unspecified -> Pbrt.Encoder.int_as_varint (0) encoder
  | Span_kind_internal -> Pbrt.Encoder.int_as_varint 1 encoder
  | Span_kind_server -> Pbrt.Encoder.int_as_varint 2 encoder
  | Span_kind_client -> Pbrt.Encoder.int_as_varint 3 encoder
  | Span_kind_producer -> Pbrt.Encoder.int_as_varint 4 encoder
  | Span_kind_consumer -> Pbrt.Encoder.int_as_varint 5 encoder

let rec encode_pb_span_event (v:span_event) encoder = 
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
    Pbrt.Encoder.key 1 Pbrt.Bits64 encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 1) then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  if (Pbrt.Bitfield.get v._presence 2) then (
    Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
    Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_span_link (v:span_link) encoder = 
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.bytes v.trace_id encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 1) then (
    Pbrt.Encoder.bytes v.span_id encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 2) then (
    Pbrt.Encoder.string v.trace_state encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  if (Pbrt.Bitfield.get v._presence 3) then (
    Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
    Pbrt.Encoder.key 5 Pbrt.Varint encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 4) then (
    Pbrt.Encoder.int32_as_bits32 v.flags encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits32 encoder; 
  );
  ()

let rec encode_pb_status_status_code (v:status_status_code) encoder =
  match v with
  | Status_code_unset -> Pbrt.Encoder.int_as_varint (0) encoder
  | Status_code_ok -> Pbrt.Encoder.int_as_varint 1 encoder
  | Status_code_error -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_pb_status (v:status) encoder = 
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.string v.message encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 1) then (
    encode_pb_status_status_code v.code encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  );
  ()

let rec encode_pb_span (v:span) encoder = 
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.bytes v.trace_id encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 1) then (
    Pbrt.Encoder.bytes v.span_id encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 2) then (
    Pbrt.Encoder.string v.trace_state encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 3) then (
    Pbrt.Encoder.bytes v.parent_span_id encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 4) then (
    Pbrt.Encoder.int32_as_bits32 v.flags encoder;
    Pbrt.Encoder.key 16 Pbrt.Bits32 encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 5) then (
    Pbrt.Encoder.string v.name encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 6) then (
    encode_pb_span_span_kind v.kind encoder;
    Pbrt.Encoder.key 6 Pbrt.Varint encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 7) then (
    Pbrt.Encoder.int64_as_bits64 v.start_time_unix_nano encoder;
    Pbrt.Encoder.key 7 Pbrt.Bits64 encoder; 
  );
  if (Pbrt.Bitfield.get v._presence 8) then (
    Pbrt.Encoder.int64_as_bits64 v.end_time_unix_nano encoder;
    Pbrt.Encoder.key 8 Pbrt.Bits64 encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  if (Pbrt.Bitfield.get v._presence 9) then (
    Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
    Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_span_event x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
  ) v.events encoder;
  if (Pbrt.Bitfield.get v._presence 10) then (
    Pbrt.Encoder.int32_as_varint v.dropped_events_count encoder;
    Pbrt.Encoder.key 12 Pbrt.Varint encoder; 
  );
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_span_link x encoder;
    Pbrt.Encoder.key 13 Pbrt.Bytes encoder; 
  ) v.links encoder;
  if (Pbrt.Bitfield.get v._presence 11) then (
    Pbrt.Encoder.int32_as_varint v.dropped_links_count encoder;
    Pbrt.Encoder.key 14 Pbrt.Varint encoder; 
  );
  begin match v.status with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_status x encoder;
    Pbrt.Encoder.key 15 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_scope_spans (v:scope_spans) encoder = 
  begin match v.scope with
  | Some x -> 
    Pbrt.Encoder.nested Common.encode_pb_instrumentation_scope x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_span x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.spans encoder;
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.string v.schema_url encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_resource_spans (v:resource_spans) encoder = 
  begin match v.resource with
  | Some x -> 
    Pbrt.Encoder.nested Resource.encode_pb_resource x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_scope_spans x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.scope_spans encoder;
  if (Pbrt.Bitfield.get v._presence 0) then (
    Pbrt.Encoder.string v.schema_url encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  );
  ()

let rec encode_pb_traces_data (v:traces_data) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder ->
    Pbrt.Encoder.nested encode_pb_resource_spans x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.resource_spans encoder;
  ()

let rec encode_pb_span_flags (v:span_flags) encoder =
  match v with
  | Span_flags_do_not_use -> Pbrt.Encoder.int_as_varint (0) encoder
  | Span_flags_trace_flags_mask -> Pbrt.Encoder.int_as_varint 255 encoder
  | Span_flags_context_has_is_remote_mask -> Pbrt.Encoder.int_as_varint 256 encoder
  | Span_flags_context_is_remote_mask -> Pbrt.Encoder.int_as_varint 512 encoder

[@@@ocaml.warning "-23-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_span_span_kind d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Span_kind_unspecified:span_span_kind)
  | 1 -> (Span_kind_internal:span_span_kind)
  | 2 -> (Span_kind_server:span_span_kind)
  | 3 -> (Span_kind_client:span_span_kind)
  | 4 -> (Span_kind_producer:span_span_kind)
  | 5 -> (Span_kind_consumer:span_span_kind)
  | _ -> Pbrt.Decoder.malformed_variant "span_span_kind"

let rec decode_pb_span_event d =
  let v = default_span_event () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_span_event_attributes v (List.rev v.attributes);
    ); continue__ := false
    | Some (1, Pbrt.Bits64) -> begin
      set_span_event_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      set_span_event_name v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      set_span_event_attributes v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      set_span_event_dropped_attributes_count v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : span_event)

let rec decode_pb_span_link d =
  let v = default_span_link () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_span_link_attributes v (List.rev v.attributes);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_span_link_trace_id v (Pbrt.Decoder.bytes d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      set_span_link_span_id v (Pbrt.Decoder.bytes d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      set_span_link_trace_state v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      set_span_link_attributes v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(4)" pk
    | Some (5, Pbrt.Varint) -> begin
      set_span_link_dropped_attributes_count v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(5)" pk
    | Some (6, Pbrt.Bits32) -> begin
      set_span_link_flags v (Pbrt.Decoder.int32_as_bits32 d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(6)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : span_link)

let rec decode_pb_status_status_code d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Status_code_unset:status_status_code)
  | 1 -> (Status_code_ok:status_status_code)
  | 2 -> (Status_code_error:status_status_code)
  | _ -> Pbrt.Decoder.malformed_variant "status_status_code"

let rec decode_pb_status d =
  let v = default_status () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (2, Pbrt.Bytes) -> begin
      set_status_message v (Pbrt.Decoder.string d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      set_status_code v (decode_pb_status_status_code d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : status)

let rec decode_pb_span d =
  let v = default_span () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_span_links v (List.rev v.links);
      set_span_events v (List.rev v.events);
      set_span_attributes v (List.rev v.attributes);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_span_trace_id v (Pbrt.Decoder.bytes d);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      set_span_span_id v (Pbrt.Decoder.bytes d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      set_span_trace_state v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      set_span_parent_span_id v (Pbrt.Decoder.bytes d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(4)" pk
    | Some (16, Pbrt.Bits32) -> begin
      set_span_flags v (Pbrt.Decoder.int32_as_bits32 d);
    end
    | Some (16, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(16)" pk
    | Some (5, Pbrt.Bytes) -> begin
      set_span_name v (Pbrt.Decoder.string d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(5)" pk
    | Some (6, Pbrt.Varint) -> begin
      set_span_kind v (decode_pb_span_span_kind d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(6)" pk
    | Some (7, Pbrt.Bits64) -> begin
      set_span_start_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(7)" pk
    | Some (8, Pbrt.Bits64) -> begin
      set_span_end_time_unix_nano v (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      set_span_attributes v ((Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes);
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(9)" pk
    | Some (10, Pbrt.Varint) -> begin
      set_span_dropped_attributes_count v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      set_span_events v ((decode_pb_span_event (Pbrt.Decoder.nested d)) :: v.events);
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(11)" pk
    | Some (12, Pbrt.Varint) -> begin
      set_span_dropped_events_count v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(12)" pk
    | Some (13, Pbrt.Bytes) -> begin
      set_span_links v ((decode_pb_span_link (Pbrt.Decoder.nested d)) :: v.links);
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(13)" pk
    | Some (14, Pbrt.Varint) -> begin
      set_span_dropped_links_count v (Pbrt.Decoder.int32_as_varint d);
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(14)" pk
    | Some (15, Pbrt.Bytes) -> begin
      set_span_status v (decode_pb_status (Pbrt.Decoder.nested d));
    end
    | Some (15, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(15)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : span)

let rec decode_pb_scope_spans d =
  let v = default_scope_spans () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_scope_spans_spans v (List.rev v.spans);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_scope_spans_scope v (Common.decode_pb_instrumentation_scope (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_spans), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      set_scope_spans_spans v ((decode_pb_span (Pbrt.Decoder.nested d)) :: v.spans);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_spans), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      set_scope_spans_schema_url v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_spans), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : scope_spans)

let rec decode_pb_resource_spans d =
  let v = default_resource_spans () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_resource_spans_scope_spans v (List.rev v.scope_spans);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_resource_spans_resource v (Resource.decode_pb_resource (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_spans), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      set_resource_spans_scope_spans v ((decode_pb_scope_spans (Pbrt.Decoder.nested d)) :: v.scope_spans);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_spans), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      set_resource_spans_schema_url v (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_spans), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : resource_spans)

let rec decode_pb_traces_data d =
  let v = default_traces_data () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      (* put lists in the correct order *)
      set_traces_data_resource_spans v (List.rev v.resource_spans);
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      set_traces_data_resource_spans v ((decode_pb_resource_spans (Pbrt.Decoder.nested d)) :: v.resource_spans);
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(traces_data), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  (v : traces_data)

let rec decode_pb_span_flags d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Span_flags_do_not_use:span_flags)
  | 255 -> (Span_flags_trace_flags_mask:span_flags)
  | 256 -> (Span_flags_context_has_is_remote_mask:span_flags)
  | 512 -> (Span_flags_context_is_remote_mask:span_flags)
  | _ -> Pbrt.Decoder.malformed_variant "span_flags"
