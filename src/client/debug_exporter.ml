open Common_
open Opentelemetry_emitter

(** [debug exporter] behaves like [exporter], but will print signals on [stderr]
    before passing them to [exporter] *)
let debug ?(out = Format.err_formatter) (exp : OTEL.Exporter.t) :
    OTEL.Exporter.t =
  let open Proto in
  {
    emit_spans =
      Emitter.tap
        (fun sp -> Format.fprintf out "SPAN: %a@." Trace.pp_span sp)
        exp.emit_spans;
    emit_logs =
      Emitter.tap
        (fun log -> Format.fprintf out "LOG: %a@." Proto.Logs.pp_log_record log)
        exp.emit_logs;
    emit_metrics =
      Emitter.tap
        (fun m -> Format.fprintf out "METRIC: %a@." Metrics.pp_metric m)
        exp.emit_metrics;
    on_tick = exp.on_tick;
    cleanup =
      (fun ~on_done () ->
        Format.fprintf out "CLEANUP@.";
        exp.cleanup ~on_done ());
  }

(** Exporter that simply debugs on [stderr] *)
let debug_only : OTEL.Exporter.t =
  debug ~out:Format.err_formatter @@ OTEL.Exporter.dummy ()
