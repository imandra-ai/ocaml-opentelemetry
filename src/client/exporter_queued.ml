(** Build an exporter from a queue and a consumer *)

open Common_
module BQ = Bounded_queue

module BQ_emitters = struct
  (* NOTE: these emitters, when closed, don't close the bounded
     queue because we need to flush_and_close the other emitters first.
     The bounded queue is a shared resource. *)

  let logs_emitter_of_bq ?service_name ?attrs
      (q : Any_resource.t Bounded_queue.t) : OTEL.Logger.t =
    Bounded_queue.to_emitter q ~close_queue_on_close:false
    |> Opentelemetry_emitter.Emitter.flat_map
         (Any_resource.of_logs_or_empty ?service_name ?attrs)

  let spans_emitter_of_bq ?service_name ?attrs
      (q : Any_resource.t Bounded_queue.t) : OTEL.Tracer.t =
    Bounded_queue.to_emitter q ~close_queue_on_close:false
    |> Opentelemetry_emitter.Emitter.flat_map
         (Any_resource.of_spans_or_empty ?service_name ?attrs)

  let metrics_emitter_of_bq ?service_name ?attrs
      (q : Any_resource.t Bounded_queue.t) : OTEL.Metrics_emitter.t =
    Bounded_queue.to_emitter q ~close_queue_on_close:false
    |> Opentelemetry_emitter.Emitter.flat_map
         (Any_resource.of_metrics_or_empty ?service_name ?attrs)
end

(** Pair a queue with a consumer to build an exporter.

    The resulting exporter will emit logs, spans, and traces directly into the
    bounded queue; while the consumer takes them from the queue to forward them
    somewhere else, store them, etc.
    @param resource_attributes attributes added to every "resource" batch *)
let create ?(resource_attributes = []) ~(q : Any_resource.t Bounded_queue.t)
    ~(consumer : Consumer.any_resource_builder) () : OTEL.Exporter.t =
  let open Opentelemetry_emitter in
  let shutdown_started = Atomic.make false in
  let active, trigger = Aswitch.create () in
  let consumer = consumer.start_consuming q in

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

  let shutdown () =
    if Aswitch.is_on active && not (Atomic.exchange shutdown_started true) then (
      (* flush all emitters *)
      Emitter.flush_and_close emit_spans;
      Emitter.flush_and_close emit_logs;
      Emitter.flush_and_close emit_metrics;

      (* first, prevent further pushes to the queue. Consumer workers
       can still drain it. *)
      Bounded_queue.close q;

      (* shutdown consumer; once it's down it'll turn our switch off too *)
      Aswitch.link (Consumer.active consumer) trigger;
      Consumer.shutdown consumer
    )
  in

  (* if consumer shuts down for some reason, we also must *)
  Aswitch.on_turn_off (Consumer.active consumer) shutdown;

  let active () = active in
  { active; emit_logs; emit_metrics; emit_spans; tick; on_tick; shutdown }
