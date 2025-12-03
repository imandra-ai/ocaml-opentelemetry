open Common_

(** [debug exporter] behaves like [exporter], but will print signals on [stderr]
    before passing them to [exporter] *)
class debug ?(out = Format.err_formatter) (exp : #OTEL.Exporter.t) :
  OTEL.Exporter.t =
  let open Proto in
  object
    method send_trace l =
      Format.fprintf out "SPANS: %a@." (Format.pp_print_list Trace.pp_span) l;
      exp#send_trace l

    method send_metrics l =
      Format.fprintf out "METRICS: %a@."
        (Format.pp_print_list Metrics.pp_metric)
        l;
      exp#send_metrics l

    method send_logs l =
      Format.fprintf out "LOGS: %a@."
        (Format.pp_print_list Logs.pp_log_record)
        l;
      exp#send_logs l

    method tick () = exp#tick ()

    method add_on_tick_callback cb = exp#add_on_tick_callback cb

    method cleanup ~on_done () =
      Format.fprintf out "CLEANUP@.";
      exp#cleanup ~on_done ()
  end

(** Exporter that simply debugs on [stderr] *)
let debug_only : OTEL.Exporter.t =
  new debug ~out:Format.err_formatter OTEL.Exporter.dummy
