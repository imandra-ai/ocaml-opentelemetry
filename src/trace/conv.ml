open Common_

let[@inline] trace_id_of_otel (id : Otel.Trace_id.t) : Otrace.trace_id =
  if id == Otel.Trace_id.dummy then
    Otrace.Collector.dummy_trace_id
  else
    Bytes.unsafe_to_string (Otel.Trace_id.to_bytes id)

let[@inline] trace_id_to_otel (id : Otrace.trace_id) : Otel.Trace_id.t =
  if id == Otrace.Collector.dummy_trace_id then
    Otel.Trace_id.dummy
  else
    Otel.Trace_id.of_bytes @@ Bytes.unsafe_of_string id

let[@inline] span_id_of_otel (id : Otel.Span_id.t) : Otrace.span =
  if id == Otel.Span_id.dummy then
    Otrace.Collector.dummy_span
  else
    Bytes.get_int64_le (Otel.Span_id.to_bytes id) 0

let[@inline] span_id_to_otel (id : Otrace.span) : Otel.Span_id.t =
  if id == Otrace.Collector.dummy_span then
    Otel.Span_id.dummy
  else (
    let b = Bytes.create 8 in
    Bytes.set_int64_le b 0 id;
    Otel.Span_id.of_bytes b
  )

let[@inline] ctx_to_otel (self : Otrace.explicit_span_ctx) : Otel.Span_ctx.t =
  Otel.Span_ctx.make
    ~trace_id:(trace_id_to_otel self.trace_id)
    ~parent_id:(span_id_to_otel self.span)
    ()

let[@inline] ctx_of_otel (ctx : Otel.Span_ctx.t) : Otrace.explicit_span_ctx =
  {
    trace_id = trace_id_of_otel (Otel.Span_ctx.trace_id ctx);
    span = span_id_of_otel (Otel.Span_ctx.parent_id ctx);
  }
