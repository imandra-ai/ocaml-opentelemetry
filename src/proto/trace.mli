
(** Code for trace.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/trace/v1/trace.proto", do not edit *)



(** {2 Types} *)

type span_span_kind =
  | Span_kind_unspecified 
  | Span_kind_internal 
  | Span_kind_server 
  | Span_kind_client 
  | Span_kind_producer 
  | Span_kind_consumer 

type span_event = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 3 fields *)
  mutable time_unix_nano : int64;
  mutable name : string;
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
}

type span_link = private {
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

type status = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 2 fields *)
  mutable message : string;
  mutable code : status_status_code;
}

type span = private {
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

type scope_spans = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable scope : Common.instrumentation_scope option;
  mutable spans : span list;
  mutable schema_url : string;
}

type resource_spans = private {
  mutable _presence: Pbrt.Bitfield.t; (** presence for 1 fields *)
  mutable resource : Resource.resource option;
  mutable scope_spans : scope_spans list;
  mutable schema_url : string;
}

type traces_data = private {
  mutable resource_spans : resource_spans list;
}

type span_flags =
  | Span_flags_do_not_use 
  | Span_flags_trace_flags_mask 
  | Span_flags_context_has_is_remote_mask 
  | Span_flags_context_is_remote_mask 


(** {2 Basic values} *)

val default_span_span_kind : unit -> span_span_kind
(** [default_span_span_kind ()] is a new empty value for type [span_span_kind] *)

val default_span_event : unit -> span_event 
(** [default_span_event ()] is a new empty value for type [span_event] *)

val default_span_link : unit -> span_link 
(** [default_span_link ()] is a new empty value for type [span_link] *)

val default_status_status_code : unit -> status_status_code
(** [default_status_status_code ()] is a new empty value for type [status_status_code] *)

val default_status : unit -> status 
(** [default_status ()] is a new empty value for type [status] *)

val default_span : unit -> span 
(** [default_span ()] is a new empty value for type [span] *)

val default_scope_spans : unit -> scope_spans 
(** [default_scope_spans ()] is a new empty value for type [scope_spans] *)

val default_resource_spans : unit -> resource_spans 
(** [default_resource_spans ()] is a new empty value for type [resource_spans] *)

val default_traces_data : unit -> traces_data 
(** [default_traces_data ()] is a new empty value for type [traces_data] *)

val default_span_flags : unit -> span_flags
(** [default_span_flags ()] is a new empty value for type [span_flags] *)


(** {2 Make functions} *)


val make_span_event : 
  ?time_unix_nano:int64 ->
  ?name:string ->
  ?attributes:Common.key_value list ->
  ?dropped_attributes_count:int32 ->
  unit ->
  span_event
(** [make_span_event … ()] is a builder for type [span_event] *)

val copy_span_event : span_event -> span_event

val span_event_has_time_unix_nano : span_event -> bool
  (** presence of field "time_unix_nano" in [span_event] *)

val span_event_set_time_unix_nano : span_event -> int64 -> unit
  (** set field time_unix_nano in span_event *)

val span_event_has_name : span_event -> bool
  (** presence of field "name" in [span_event] *)

val span_event_set_name : span_event -> string -> unit
  (** set field name in span_event *)

val span_event_set_attributes : span_event -> Common.key_value list -> unit
  (** set field attributes in span_event *)

val span_event_has_dropped_attributes_count : span_event -> bool
  (** presence of field "dropped_attributes_count" in [span_event] *)

val span_event_set_dropped_attributes_count : span_event -> int32 -> unit
  (** set field dropped_attributes_count in span_event *)

val make_span_link : 
  ?trace_id:bytes ->
  ?span_id:bytes ->
  ?trace_state:string ->
  ?attributes:Common.key_value list ->
  ?dropped_attributes_count:int32 ->
  ?flags:int32 ->
  unit ->
  span_link
(** [make_span_link … ()] is a builder for type [span_link] *)

val copy_span_link : span_link -> span_link

val span_link_has_trace_id : span_link -> bool
  (** presence of field "trace_id" in [span_link] *)

val span_link_set_trace_id : span_link -> bytes -> unit
  (** set field trace_id in span_link *)

val span_link_has_span_id : span_link -> bool
  (** presence of field "span_id" in [span_link] *)

val span_link_set_span_id : span_link -> bytes -> unit
  (** set field span_id in span_link *)

val span_link_has_trace_state : span_link -> bool
  (** presence of field "trace_state" in [span_link] *)

val span_link_set_trace_state : span_link -> string -> unit
  (** set field trace_state in span_link *)

val span_link_set_attributes : span_link -> Common.key_value list -> unit
  (** set field attributes in span_link *)

val span_link_has_dropped_attributes_count : span_link -> bool
  (** presence of field "dropped_attributes_count" in [span_link] *)

val span_link_set_dropped_attributes_count : span_link -> int32 -> unit
  (** set field dropped_attributes_count in span_link *)

val span_link_has_flags : span_link -> bool
  (** presence of field "flags" in [span_link] *)

val span_link_set_flags : span_link -> int32 -> unit
  (** set field flags in span_link *)


val make_status : 
  ?message:string ->
  ?code:status_status_code ->
  unit ->
  status
(** [make_status … ()] is a builder for type [status] *)

val copy_status : status -> status

val status_has_message : status -> bool
  (** presence of field "message" in [status] *)

val status_set_message : status -> string -> unit
  (** set field message in status *)

val status_has_code : status -> bool
  (** presence of field "code" in [status] *)

val status_set_code : status -> status_status_code -> unit
  (** set field code in status *)

val make_span : 
  ?trace_id:bytes ->
  ?span_id:bytes ->
  ?trace_state:string ->
  ?parent_span_id:bytes ->
  ?flags:int32 ->
  ?name:string ->
  ?kind:span_span_kind ->
  ?start_time_unix_nano:int64 ->
  ?end_time_unix_nano:int64 ->
  ?attributes:Common.key_value list ->
  ?dropped_attributes_count:int32 ->
  ?events:span_event list ->
  ?dropped_events_count:int32 ->
  ?links:span_link list ->
  ?dropped_links_count:int32 ->
  ?status:status ->
  unit ->
  span
(** [make_span … ()] is a builder for type [span] *)

val copy_span : span -> span

val span_has_trace_id : span -> bool
  (** presence of field "trace_id" in [span] *)

val span_set_trace_id : span -> bytes -> unit
  (** set field trace_id in span *)

val span_has_span_id : span -> bool
  (** presence of field "span_id" in [span] *)

val span_set_span_id : span -> bytes -> unit
  (** set field span_id in span *)

val span_has_trace_state : span -> bool
  (** presence of field "trace_state" in [span] *)

val span_set_trace_state : span -> string -> unit
  (** set field trace_state in span *)

val span_has_parent_span_id : span -> bool
  (** presence of field "parent_span_id" in [span] *)

val span_set_parent_span_id : span -> bytes -> unit
  (** set field parent_span_id in span *)

val span_has_flags : span -> bool
  (** presence of field "flags" in [span] *)

val span_set_flags : span -> int32 -> unit
  (** set field flags in span *)

val span_has_name : span -> bool
  (** presence of field "name" in [span] *)

val span_set_name : span -> string -> unit
  (** set field name in span *)

val span_has_kind : span -> bool
  (** presence of field "kind" in [span] *)

val span_set_kind : span -> span_span_kind -> unit
  (** set field kind in span *)

val span_has_start_time_unix_nano : span -> bool
  (** presence of field "start_time_unix_nano" in [span] *)

val span_set_start_time_unix_nano : span -> int64 -> unit
  (** set field start_time_unix_nano in span *)

val span_has_end_time_unix_nano : span -> bool
  (** presence of field "end_time_unix_nano" in [span] *)

val span_set_end_time_unix_nano : span -> int64 -> unit
  (** set field end_time_unix_nano in span *)

val span_set_attributes : span -> Common.key_value list -> unit
  (** set field attributes in span *)

val span_has_dropped_attributes_count : span -> bool
  (** presence of field "dropped_attributes_count" in [span] *)

val span_set_dropped_attributes_count : span -> int32 -> unit
  (** set field dropped_attributes_count in span *)

val span_set_events : span -> span_event list -> unit
  (** set field events in span *)

val span_has_dropped_events_count : span -> bool
  (** presence of field "dropped_events_count" in [span] *)

val span_set_dropped_events_count : span -> int32 -> unit
  (** set field dropped_events_count in span *)

val span_set_links : span -> span_link list -> unit
  (** set field links in span *)

val span_has_dropped_links_count : span -> bool
  (** presence of field "dropped_links_count" in [span] *)

val span_set_dropped_links_count : span -> int32 -> unit
  (** set field dropped_links_count in span *)

val span_set_status : span -> status -> unit
  (** set field status in span *)

val make_scope_spans : 
  ?scope:Common.instrumentation_scope ->
  ?spans:span list ->
  ?schema_url:string ->
  unit ->
  scope_spans
(** [make_scope_spans … ()] is a builder for type [scope_spans] *)

val copy_scope_spans : scope_spans -> scope_spans

val scope_spans_set_scope : scope_spans -> Common.instrumentation_scope -> unit
  (** set field scope in scope_spans *)

val scope_spans_set_spans : scope_spans -> span list -> unit
  (** set field spans in scope_spans *)

val scope_spans_has_schema_url : scope_spans -> bool
  (** presence of field "schema_url" in [scope_spans] *)

val scope_spans_set_schema_url : scope_spans -> string -> unit
  (** set field schema_url in scope_spans *)

val make_resource_spans : 
  ?resource:Resource.resource ->
  ?scope_spans:scope_spans list ->
  ?schema_url:string ->
  unit ->
  resource_spans
(** [make_resource_spans … ()] is a builder for type [resource_spans] *)

val copy_resource_spans : resource_spans -> resource_spans

val resource_spans_set_resource : resource_spans -> Resource.resource -> unit
  (** set field resource in resource_spans *)

val resource_spans_set_scope_spans : resource_spans -> scope_spans list -> unit
  (** set field scope_spans in resource_spans *)

val resource_spans_has_schema_url : resource_spans -> bool
  (** presence of field "schema_url" in [resource_spans] *)

val resource_spans_set_schema_url : resource_spans -> string -> unit
  (** set field schema_url in resource_spans *)

val make_traces_data : 
  ?resource_spans:resource_spans list ->
  unit ->
  traces_data
(** [make_traces_data … ()] is a builder for type [traces_data] *)

val copy_traces_data : traces_data -> traces_data

val traces_data_set_resource_spans : traces_data -> resource_spans list -> unit
  (** set field resource_spans in traces_data *)



(** {2 Formatters} *)

val pp_span_span_kind : Format.formatter -> span_span_kind -> unit 
(** [pp_span_span_kind v] formats v *)

val pp_span_event : Format.formatter -> span_event -> unit 
(** [pp_span_event v] formats v *)

val pp_span_link : Format.formatter -> span_link -> unit 
(** [pp_span_link v] formats v *)

val pp_status_status_code : Format.formatter -> status_status_code -> unit 
(** [pp_status_status_code v] formats v *)

val pp_status : Format.formatter -> status -> unit 
(** [pp_status v] formats v *)

val pp_span : Format.formatter -> span -> unit 
(** [pp_span v] formats v *)

val pp_scope_spans : Format.formatter -> scope_spans -> unit 
(** [pp_scope_spans v] formats v *)

val pp_resource_spans : Format.formatter -> resource_spans -> unit 
(** [pp_resource_spans v] formats v *)

val pp_traces_data : Format.formatter -> traces_data -> unit 
(** [pp_traces_data v] formats v *)

val pp_span_flags : Format.formatter -> span_flags -> unit 
(** [pp_span_flags v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_span_span_kind : span_span_kind -> Pbrt.Encoder.t -> unit
(** [encode_pb_span_span_kind v encoder] encodes [v] with the given [encoder] *)

val encode_pb_span_event : span_event -> Pbrt.Encoder.t -> unit
(** [encode_pb_span_event v encoder] encodes [v] with the given [encoder] *)

val encode_pb_span_link : span_link -> Pbrt.Encoder.t -> unit
(** [encode_pb_span_link v encoder] encodes [v] with the given [encoder] *)

val encode_pb_status_status_code : status_status_code -> Pbrt.Encoder.t -> unit
(** [encode_pb_status_status_code v encoder] encodes [v] with the given [encoder] *)

val encode_pb_status : status -> Pbrt.Encoder.t -> unit
(** [encode_pb_status v encoder] encodes [v] with the given [encoder] *)

val encode_pb_span : span -> Pbrt.Encoder.t -> unit
(** [encode_pb_span v encoder] encodes [v] with the given [encoder] *)

val encode_pb_scope_spans : scope_spans -> Pbrt.Encoder.t -> unit
(** [encode_pb_scope_spans v encoder] encodes [v] with the given [encoder] *)

val encode_pb_resource_spans : resource_spans -> Pbrt.Encoder.t -> unit
(** [encode_pb_resource_spans v encoder] encodes [v] with the given [encoder] *)

val encode_pb_traces_data : traces_data -> Pbrt.Encoder.t -> unit
(** [encode_pb_traces_data v encoder] encodes [v] with the given [encoder] *)

val encode_pb_span_flags : span_flags -> Pbrt.Encoder.t -> unit
(** [encode_pb_span_flags v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_span_span_kind : Pbrt.Decoder.t -> span_span_kind
(** [decode_pb_span_span_kind decoder] decodes a [span_span_kind] binary value from [decoder] *)

val decode_pb_span_event : Pbrt.Decoder.t -> span_event
(** [decode_pb_span_event decoder] decodes a [span_event] binary value from [decoder] *)

val decode_pb_span_link : Pbrt.Decoder.t -> span_link
(** [decode_pb_span_link decoder] decodes a [span_link] binary value from [decoder] *)

val decode_pb_status_status_code : Pbrt.Decoder.t -> status_status_code
(** [decode_pb_status_status_code decoder] decodes a [status_status_code] binary value from [decoder] *)

val decode_pb_status : Pbrt.Decoder.t -> status
(** [decode_pb_status decoder] decodes a [status] binary value from [decoder] *)

val decode_pb_span : Pbrt.Decoder.t -> span
(** [decode_pb_span decoder] decodes a [span] binary value from [decoder] *)

val decode_pb_scope_spans : Pbrt.Decoder.t -> scope_spans
(** [decode_pb_scope_spans decoder] decodes a [scope_spans] binary value from [decoder] *)

val decode_pb_resource_spans : Pbrt.Decoder.t -> resource_spans
(** [decode_pb_resource_spans decoder] decodes a [resource_spans] binary value from [decoder] *)

val decode_pb_traces_data : Pbrt.Decoder.t -> traces_data
(** [decode_pb_traces_data decoder] decodes a [traces_data] binary value from [decoder] *)

val decode_pb_span_flags : Pbrt.Decoder.t -> span_flags
(** [decode_pb_span_flags decoder] decodes a [span_flags] binary value from [decoder] *)
