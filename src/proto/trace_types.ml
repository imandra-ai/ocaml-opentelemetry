[@@@ocaml.warning "-27-30-39"]


type span_span_kind =
  | Span_kind_unspecified 
  | Span_kind_internal 
  | Span_kind_server 
  | Span_kind_client 
  | Span_kind_producer 
  | Span_kind_consumer 

type span_event = {
  time_unix_nano : int64;
  name : string;
  attributes : Common_types.key_value list;
  dropped_attributes_count : int32;
}

type span_link = {
  trace_id : bytes;
  span_id : bytes;
  trace_state : string;
  attributes : Common_types.key_value list;
  dropped_attributes_count : int32;
}

type status_status_code =
  | Status_code_unset 
  | Status_code_ok 
  | Status_code_error 

type status = {
  message : string;
  code : status_status_code;
}

type span = {
  trace_id : bytes;
  span_id : bytes;
  trace_state : string;
  parent_span_id : bytes;
  name : string;
  kind : span_span_kind;
  start_time_unix_nano : int64;
  end_time_unix_nano : int64;
  attributes : Common_types.key_value list;
  dropped_attributes_count : int32;
  events : span_event list;
  dropped_events_count : int32;
  links : span_link list;
  dropped_links_count : int32;
  status : status option;
}

type scope_spans = {
  scope : Common_types.instrumentation_scope option;
  spans : span list;
  schema_url : string;
}

type resource_spans = {
  resource : Resource_types.resource option;
  scope_spans : scope_spans list;
  schema_url : string;
}

type traces_data = {
  resource_spans : resource_spans list;
}

let rec default_span_span_kind () = (Span_kind_unspecified:span_span_kind)

let rec default_span_event 
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?name:((name:string) = "")
  ?attributes:((attributes:Common_types.key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  () : span_event  = {
  time_unix_nano;
  name;
  attributes;
  dropped_attributes_count;
}

let rec default_span_link 
  ?trace_id:((trace_id:bytes) = Bytes.create 0)
  ?span_id:((span_id:bytes) = Bytes.create 0)
  ?trace_state:((trace_state:string) = "")
  ?attributes:((attributes:Common_types.key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  () : span_link  = {
  trace_id;
  span_id;
  trace_state;
  attributes;
  dropped_attributes_count;
}

let rec default_status_status_code () = (Status_code_unset:status_status_code)

let rec default_status 
  ?message:((message:string) = "")
  ?code:((code:status_status_code) = default_status_status_code ())
  () : status  = {
  message;
  code;
}

let rec default_span 
  ?trace_id:((trace_id:bytes) = Bytes.create 0)
  ?span_id:((span_id:bytes) = Bytes.create 0)
  ?trace_state:((trace_state:string) = "")
  ?parent_span_id:((parent_span_id:bytes) = Bytes.create 0)
  ?name:((name:string) = "")
  ?kind:((kind:span_span_kind) = default_span_span_kind ())
  ?start_time_unix_nano:((start_time_unix_nano:int64) = 0L)
  ?end_time_unix_nano:((end_time_unix_nano:int64) = 0L)
  ?attributes:((attributes:Common_types.key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  ?events:((events:span_event list) = [])
  ?dropped_events_count:((dropped_events_count:int32) = 0l)
  ?links:((links:span_link list) = [])
  ?dropped_links_count:((dropped_links_count:int32) = 0l)
  ?status:((status:status option) = None)
  () : span  = {
  trace_id;
  span_id;
  trace_state;
  parent_span_id;
  name;
  kind;
  start_time_unix_nano;
  end_time_unix_nano;
  attributes;
  dropped_attributes_count;
  events;
  dropped_events_count;
  links;
  dropped_links_count;
  status;
}

let rec default_scope_spans 
  ?scope:((scope:Common_types.instrumentation_scope option) = None)
  ?spans:((spans:span list) = [])
  ?schema_url:((schema_url:string) = "")
  () : scope_spans  = {
  scope;
  spans;
  schema_url;
}

let rec default_resource_spans 
  ?resource:((resource:Resource_types.resource option) = None)
  ?scope_spans:((scope_spans:scope_spans list) = [])
  ?schema_url:((schema_url:string) = "")
  () : resource_spans  = {
  resource;
  scope_spans;
  schema_url;
}

let rec default_traces_data 
  ?resource_spans:((resource_spans:resource_spans list) = [])
  () : traces_data  = {
  resource_spans;
}
