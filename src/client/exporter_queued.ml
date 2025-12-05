(** Build an exporter from a queue and a consumer *)

open Common_
module BQ = Bounded_queue

module BQ_emitters = struct
  let logs_emitter_of_bq ?service_name ?attrs
      (q : Any_resource.t Bounded_queue.t) : OTEL.Logger.t =
    Bounded_queue.to_emitter q
    |> Opentelemetry_emitter.Emitter.flat_map
         (Any_resource.of_logs_or_empty ?service_name ?attrs)

  let spans_emitter_of_bq ?service_name ?attrs
      (q : Any_resource.t Bounded_queue.t) : OTEL.Tracer.t =
    Bounded_queue.to_emitter q
    |> Opentelemetry_emitter.Emitter.flat_map
         (Any_resource.of_spans_or_empty ?service_name ?attrs)

  let metrics_emitter_of_bq ?service_name ?attrs
      (q : Any_resource.t Bounded_queue.t) : OTEL.Metrics_emitter.t =
    Bounded_queue.to_emitter q
    |> Opentelemetry_emitter.Emitter.flat_map
         (Any_resource.of_metrics_or_empty ?service_name ?attrs)
end

(** Pair a queue with a consumer to build an exporter.

    The resulting exporter will emit logs, spans, and traces directly into the
    bounded queue; while the consumer takes them from the queue to forward them
    somewhere else, store them, etc.
    @param resource_attributes attributes added to every "resource" batch *)
let create ?(resource_attributes = []) ~(q : Any_resource.t Bounded_queue.t)
    ~(consumer : Any_resource.t Consumer.t) () : OTEL.Exporter.t =
  let emit_spans =
    BQ_emitters.spans_emitter_of_bq ~attrs:resource_attributes q
  in
  let emit_logs = BQ_emitters.logs_emitter_of_bq ~attrs:resource_attributes q in
  let emit_metrics =
    BQ_emitters.metrics_emitter_of_bq ~attrs:resource_attributes q
  in

  let tick_set = Cb_set.create () in
  let tick () = Cb_set.trigger tick_set in
  let on_tick f = Cb_set.register tick_set f in

  let closed = Atomic.make false in

  let cleanup ~on_done () =
    if not (Atomic.exchange closed true) then (
      Bounded_queue.close q;
      Consumer.shutdown consumer ~on_done
    ) else
      on_done ()
  in
  { emit_logs; emit_metrics; emit_spans; tick; on_tick; cleanup }
