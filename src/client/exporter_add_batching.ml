(** Add batching to emitters *)

open Common_

open struct
  let add_batch ~timeout batch (emitter : 'a OTEL.Emitter.t) : 'a OTEL.Emitter.t
      =
    let b = Batch.make ~batch ~timeout () in
    Batch.wrap_emitter b emitter
end

(** Given an exporter, add batches for each emitter according to [config]. *)
let add_batching ~(config : Client_config.t) (exp : OTEL.Exporter.t) :
    OTEL.Exporter.t =
  let timeout = Mtime.Span.(config.batch_timeout_ms * ms) in
  let add_batch_opt (b : int option) e =
    match b with
    | None -> e
    | Some b -> add_batch ~timeout b e
  in

  let emit_spans = add_batch_opt config.batch_traces exp.emit_spans in
  let emit_metrics = add_batch_opt config.batch_metrics exp.emit_metrics in
  let emit_logs = add_batch_opt config.batch_logs exp.emit_logs in

  { exp with emit_spans; emit_metrics; emit_logs }
