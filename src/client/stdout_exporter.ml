(** A simple exporter that prints on stdout *)

open Common_
open Opentelemetry_util

open struct
  let pp_span out (sp : OTEL.Span.t) =
    let open OTEL in
    Format.fprintf out
      "@[<2>SPAN@ trace_id: %a@ span_id: %a@ name: %S@ start: %a@ end: %a@]@."
      Trace_id.pp
      (Trace_id.of_bytes sp.trace_id)
      Span_id.pp
      (Span_id.of_bytes sp.span_id)
      sp.name Timestamp_ns.pp_debug sp.start_time_unix_nano
      Timestamp_ns.pp_debug sp.end_time_unix_nano

  let pp_vlist mutex pp out l =
    if l != [] then (
      let@ () = Util_mutex.protect mutex in
      Format.fprintf out "@[<v>";
      List.iteri
        (fun i x ->
          if i > 0 then Format.fprintf out "@,";
          pp out x)
        l;
      Format.fprintf out "@]@."
    )
end

class stdout : OTEL.Exporter.t =
  let open Opentelemetry_util in
  let out = Format.std_formatter in
  let mutex = Mutex.create () in

  let tick_cbs = Cb_set.create () in
  object
    method send_trace l = pp_vlist mutex pp_span out l

    method send_metrics l = pp_vlist mutex Proto.Metrics.pp_metric out l

    method send_logs l = pp_vlist mutex Proto.Logs.pp_log_record out l

    method tick () = Cb_set.trigger tick_cbs

    method add_on_tick_callback cb = Cb_set.register tick_cbs cb

    method cleanup ~on_done () = on_done ()
  end
