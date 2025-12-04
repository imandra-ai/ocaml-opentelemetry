open Common_

let enabled = Atomic.make false

let tracer = Atomic.make OTEL.Tracer.dynamic_forward_to_main_exporter

let[@inline] add_event (scope : OTEL.Span.t) ev = OTEL.Span.add_event scope ev

let set_tracer tr = Atomic.set tracer tr

let dummy_trace_id_ = OTEL.Trace_id.dummy

let dummy_span_id = OTEL.Span_id.dummy

let with_ ?kind ?attrs name f =
  if Atomic.get enabled then (
    let tracer = Atomic.get tracer in
    OTEL.Tracer.with_ tracer ?kind ?attrs name f
  ) else (
    (* A new scope is needed here because it might be modified *)
    let span : OTEL.Span.t =
      OTEL.Span.make ~trace_id:dummy_trace_id_ ~id:dummy_span_id ~start_time:0L
        ~end_time:0L name
    in
    f span
  )

let set_enabled b = Atomic.set enabled b
