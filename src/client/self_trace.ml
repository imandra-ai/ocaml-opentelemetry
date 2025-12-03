module OT = Opentelemetry

let enabled = Atomic.make false

let add_event (scope : OT.Scope.t) ev = OT.Scope.add_event scope (fun () -> ev)

let dummy_trace_id_ = OT.Trace_id.dummy

let dummy_span_id = OT.Span_id.dummy

(* FIXME: get an explicit tracer instead *)
let with_ ?kind ?attrs name f =
  if Atomic.get enabled then
    OT.Tracer.with_ ?kind ?attrs name f
  else (
    (* A new scope is needed here because it might be modified *)
    let scope =
      OT.Scope.make ~trace_id:dummy_trace_id_ ~span_id:dummy_span_id ()
    in
    f scope
  )

let set_enabled b = Atomic.set enabled b
