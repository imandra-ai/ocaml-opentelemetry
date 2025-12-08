open Common_
open Proto.Trace

type t = span_link

let make ~trace_id ~span_id ?trace_state ?(attrs = []) ?dropped_attributes_count
    () : t =
  let attributes = List.map Key_value.conv attrs in
  let dropped_attributes_count =
    Option.map Int32.of_int dropped_attributes_count
  in
  make_span_link
    ~trace_id:(Trace_id.to_bytes trace_id)
    ~span_id:(Span_id.to_bytes span_id) ?trace_state ~attributes
    ?dropped_attributes_count ()

let[@inline] of_span_ctx ?trace_state ?attrs ?dropped_attributes_count
    (ctx : Span_ctx.t) : t =
  make ~trace_id:(Span_ctx.trace_id ctx) ~span_id:(Span_ctx.parent_id ctx)
    ?trace_state ?attrs ?dropped_attributes_count ()
