open Common_

let enabled = Atomic.make false

let[@inline] add_event (scope : OTEL.Span.t) ev = OTEL.Span.add_event scope ev

let dummy_trace_id_ = OTEL.Trace_id.dummy

let dummy_span_id = OTEL.Span_id.dummy

(* FIXME: get an explicit tracer instead *)
let with_ ?kind ?attrs name f =
  if Atomic.get enabled then
    OTEL.Tracer.with_ ?kind ?attrs name f
  else (
    (* A new scope is needed here because it might be modified *)
    let span : OTEL.Span.t =
      OTEL.Span.make ~trace_id:dummy_trace_id_ ~id:dummy_span_id ~start_time:0L
        ~end_time:0L name
    in
    f span
  )

let set_enabled b = Atomic.set enabled b
