[@@@ocaml.warning "-27-30-39"]

let rec pp_span_span_kind fmt (v:Trace_types.span_span_kind) =
  match v with
  | Trace_types.Span_kind_unspecified -> Format.fprintf fmt "Span_kind_unspecified"
  | Trace_types.Span_kind_internal -> Format.fprintf fmt "Span_kind_internal"
  | Trace_types.Span_kind_server -> Format.fprintf fmt "Span_kind_server"
  | Trace_types.Span_kind_client -> Format.fprintf fmt "Span_kind_client"
  | Trace_types.Span_kind_producer -> Format.fprintf fmt "Span_kind_producer"
  | Trace_types.Span_kind_consumer -> Format.fprintf fmt "Span_kind_consumer"

let rec pp_span_event fmt (v:Trace_types.span_event) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Trace_types.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "name" Pbrt.Pp.pp_string fmt v.Trace_types.name;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common_pp.pp_key_value) fmt v.Trace_types.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.Trace_types.dropped_attributes_count;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_span_link fmt (v:Trace_types.span_link) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "trace_id" Pbrt.Pp.pp_bytes fmt v.Trace_types.trace_id;
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.Trace_types.span_id;
    Pbrt.Pp.pp_record_field ~first:false "trace_state" Pbrt.Pp.pp_string fmt v.Trace_types.trace_state;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common_pp.pp_key_value) fmt v.Trace_types.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.Trace_types.dropped_attributes_count;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_status_status_code fmt (v:Trace_types.status_status_code) =
  match v with
  | Trace_types.Status_code_unset -> Format.fprintf fmt "Status_code_unset"
  | Trace_types.Status_code_ok -> Format.fprintf fmt "Status_code_ok"
  | Trace_types.Status_code_error -> Format.fprintf fmt "Status_code_error"

let rec pp_status fmt (v:Trace_types.status) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "message" Pbrt.Pp.pp_string fmt v.Trace_types.message;
    Pbrt.Pp.pp_record_field ~first:false "code" pp_status_status_code fmt v.Trace_types.code;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_span fmt (v:Trace_types.span) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "trace_id" Pbrt.Pp.pp_bytes fmt v.Trace_types.trace_id;
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.Trace_types.span_id;
    Pbrt.Pp.pp_record_field ~first:false "trace_state" Pbrt.Pp.pp_string fmt v.Trace_types.trace_state;
    Pbrt.Pp.pp_record_field ~first:false "parent_span_id" Pbrt.Pp.pp_bytes fmt v.Trace_types.parent_span_id;
    Pbrt.Pp.pp_record_field ~first:false "name" Pbrt.Pp.pp_string fmt v.Trace_types.name;
    Pbrt.Pp.pp_record_field ~first:false "kind" pp_span_span_kind fmt v.Trace_types.kind;
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Trace_types.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "end_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.Trace_types.end_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common_pp.pp_key_value) fmt v.Trace_types.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.Trace_types.dropped_attributes_count;
    Pbrt.Pp.pp_record_field ~first:false "events" (Pbrt.Pp.pp_list pp_span_event) fmt v.Trace_types.events;
    Pbrt.Pp.pp_record_field ~first:false "dropped_events_count" Pbrt.Pp.pp_int32 fmt v.Trace_types.dropped_events_count;
    Pbrt.Pp.pp_record_field ~first:false "links" (Pbrt.Pp.pp_list pp_span_link) fmt v.Trace_types.links;
    Pbrt.Pp.pp_record_field ~first:false "dropped_links_count" Pbrt.Pp.pp_int32 fmt v.Trace_types.dropped_links_count;
    Pbrt.Pp.pp_record_field ~first:false "status" (Pbrt.Pp.pp_option pp_status) fmt v.Trace_types.status;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_scope_spans fmt (v:Trace_types.scope_spans) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "scope" (Pbrt.Pp.pp_option Common_pp.pp_instrumentation_scope) fmt v.Trace_types.scope;
    Pbrt.Pp.pp_record_field ~first:false "spans" (Pbrt.Pp.pp_list pp_span) fmt v.Trace_types.spans;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.Trace_types.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_resource_spans fmt (v:Trace_types.resource_spans) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource" (Pbrt.Pp.pp_option Resource_pp.pp_resource) fmt v.Trace_types.resource;
    Pbrt.Pp.pp_record_field ~first:false "scope_spans" (Pbrt.Pp.pp_list pp_scope_spans) fmt v.Trace_types.scope_spans;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.Trace_types.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_traces_data fmt (v:Trace_types.traces_data) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_spans" (Pbrt.Pp.pp_list pp_resource_spans) fmt v.Trace_types.resource_spans;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
