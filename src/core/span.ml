open Common_
open Proto.Trace

type t = span

type id = Span_id.t

type kind = Span_kind.t =
  | Span_kind_unspecified
  | Span_kind_internal
  | Span_kind_server
  | Span_kind_client
  | Span_kind_producer
  | Span_kind_consumer

type key_value =
  string
  * [ `Int of int
    | `String of string
    | `Bool of bool
    | `Float of float
    | `None
    ]

let id self = Span_id.of_bytes self.span_id

let create ?(kind = !Globals.default_span_kind) ?(id = Span_id.create ())
    ?trace_state ?(attrs = []) ?(events = []) ?status ~trace_id ?parent
    ?(links = []) ~start_time ~end_time name : t * id =
  let trace_id = Trace_id.to_bytes trace_id in
  let parent_span_id = Option.map Span_id.to_bytes parent in
  let attributes = List.map Key_value.conv attrs in
  let span =
    make_span ~trace_id ?parent_span_id ~span_id:(Span_id.to_bytes id)
      ~attributes ~events ?trace_state ?status ~kind ~name ~links
      ~start_time_unix_nano:start_time ~end_time_unix_nano:end_time ()
  in
  span, id
