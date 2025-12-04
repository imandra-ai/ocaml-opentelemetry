module OT = Opentelemetry

let enabled = Atomic.make false

let[@inline] add_event (scope : OT.Span.t) ev = OT.Span.add_event scope ev

let dummy_trace_id_ = OT.Trace_id.dummy

let dummy_span_id = OT.Span_id.dummy

(* FIXME: get an explicit tracer instead *)
let with_ ?kind ?attrs name f =
  if Atomic.get enabled then
    OT.Tracer.with_ ?kind ?attrs name f
  else (
    (* A new scope is needed here because it might be modified *)
    let span : OT.Span.t =
      OT.Span.make ~trace_id:dummy_trace_id_ ~id:dummy_span_id ~start_time:0L
        ~end_time:0L name
    in
    f span
  )

let set_enabled b = Atomic.set enabled b
