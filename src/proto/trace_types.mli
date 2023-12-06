(** trace.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_span_span_kind : unit -> span_span_kind
(** [default_span_span_kind ()] is the default value for type [span_span_kind] *)

val default_span_event : 
  ?time_unix_nano:int64 ->
  ?name:string ->
  ?attributes:Common_types.key_value list ->
  ?dropped_attributes_count:int32 ->
  unit ->
  span_event
(** [default_span_event ()] is the default value for type [span_event] *)

val default_span_link : 
  ?trace_id:bytes ->
  ?span_id:bytes ->
  ?trace_state:string ->
  ?attributes:Common_types.key_value list ->
  ?dropped_attributes_count:int32 ->
  unit ->
  span_link
(** [default_span_link ()] is the default value for type [span_link] *)

val default_status_status_code : unit -> status_status_code
(** [default_status_status_code ()] is the default value for type [status_status_code] *)

val default_status : 
  ?message:string ->
  ?code:status_status_code ->
  unit ->
  status
(** [default_status ()] is the default value for type [status] *)

val default_span : 
  ?trace_id:bytes ->
  ?span_id:bytes ->
  ?trace_state:string ->
  ?parent_span_id:bytes ->
  ?name:string ->
  ?kind:span_span_kind ->
  ?start_time_unix_nano:int64 ->
  ?end_time_unix_nano:int64 ->
  ?attributes:Common_types.key_value list ->
  ?dropped_attributes_count:int32 ->
  ?events:span_event list ->
  ?dropped_events_count:int32 ->
  ?links:span_link list ->
  ?dropped_links_count:int32 ->
  ?status:status option ->
  unit ->
  span
(** [default_span ()] is the default value for type [span] *)

val default_scope_spans : 
  ?scope:Common_types.instrumentation_scope option ->
  ?spans:span list ->
  ?schema_url:string ->
  unit ->
  scope_spans
(** [default_scope_spans ()] is the default value for type [scope_spans] *)

val default_resource_spans : 
  ?resource:Resource_types.resource option ->
  ?scope_spans:scope_spans list ->
  ?schema_url:string ->
  unit ->
  resource_spans
(** [default_resource_spans ()] is the default value for type [resource_spans] *)

val default_traces_data : 
  ?resource_spans:resource_spans list ->
  unit ->
  traces_data
(** [default_traces_data ()] is the default value for type [traces_data] *)
