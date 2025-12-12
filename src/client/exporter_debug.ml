open Common_
open Opentelemetry_emitter

(** [debug ?out ()] is an exporter that pretty-prints signals on [out].
    @param out the formatter into which to print, default [stderr]. *)
let debug ?(out = Format.err_formatter) () : OTEL.Exporter.t =
  let open Proto in
  let active, trigger = Aswitch.create () in
  let ticker = Cb_set.create () in
  {
    active = (fun () -> active);
    emit_spans =
      Emitter.make_simple () ~emit:(fun sp ->
          List.iter (Format.fprintf out "SPAN: %a@." Trace.pp_span) sp);
    emit_logs =
      Emitter.make_simple () ~emit:(fun log ->
          List.iter
            (Format.fprintf out "LOG: %a@." Proto.Logs.pp_log_record)
            log);
    emit_metrics =
      Emitter.make_simple () ~emit:(fun m ->
          List.iter (Format.fprintf out "METRIC: %a@." Metrics.pp_metric) m);
    on_tick = Cb_set.register ticker;
    tick = (fun () -> Cb_set.trigger ticker);
    self_metrics = (fun () -> []);
    shutdown =
      (fun () ->
        Format.fprintf out "CLEANUP@.";
        Aswitch.turn_off trigger);
  }
