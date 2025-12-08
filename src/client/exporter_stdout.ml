(** A simple exporter that prints on stdout *)

open Common_
open Opentelemetry_util
open Opentelemetry_emitter

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

let stdout : OTEL.Exporter.t =
  let open Opentelemetry_util in
  let out = Format.std_formatter in
  let mutex = Mutex.create () in
  let ticker = Cb_set.create () in

  let closed = Atomic.make false in
  let tick () = Cb_set.trigger ticker in

  let mk_emitter pp_signal =
    let emit l =
      if Atomic.get closed then raise Emitter.Closed;
      pp_vlist mutex pp_signal out l
    in
    let enabled () = not (Atomic.get closed) in
    let tick ~now:_ = () in
    let flush_and_close () =
      if not (Atomic.exchange closed true) then
        let@ () = Util_mutex.protect mutex in
        Format.pp_print_flush out ()
    in
    let closed () = Atomic.get closed in
    { Emitter.emit; closed; enabled; tick; flush_and_close }
  in

  let emit_spans = mk_emitter pp_span in
  let emit_logs = mk_emitter Proto.Logs.pp_log_record in
  let emit_metrics = mk_emitter Proto.Metrics.pp_metric in

  let shutdown ~on_done () =
    Emitter.flush_and_close emit_spans;
    Emitter.flush_and_close emit_logs;
    Emitter.flush_and_close emit_metrics;
    on_done ()
  in

  {
    emit_spans;
    emit_logs;
    emit_metrics;
    on_tick = Cb_set.register ticker;
    tick;
    shutdown;
  }
