module OT = Opentelemetry

let enabled = Atomic.make true

let add_event (scope : OT.Scope.t) ev = OT.Scope.add_event scope (fun () -> ev)

let dummy_trace_id_ = OT.Trace_id.create ()

let dummy_span_id = OT.Span_id.create ()

let with_ ?kind ?attrs name f =
  if Atomic.get enabled then
    OT.Trace.with_ ?kind ?attrs name f
  else (
    (* do nothing *)
    let scope =
      OT.Scope.make ~trace_id:dummy_trace_id_ ~span_id:dummy_span_id ()
    in
    f scope
  )

let set_enabled b = Atomic.set enabled b
