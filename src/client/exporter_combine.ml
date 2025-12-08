open Common_
open Opentelemetry_atomic

open struct
  let shutdown_l ~on_done:on_done_real (es : OTEL.Exporter.t list) : unit =
    let missing = Atomic.make (List.length es) in

    let on_done () =
      if Atomic.fetch_and_add missing (-1) = 1 then
        (* we were the last exporter to shutdown, [missing] is now 0 *)
        on_done_real ()
    in

    List.iter (OTEL.Exporter.shutdown ~on_done) es
end

let combine_l (es : OTEL.Exporter.t list) : OTEL.Exporter.t =
  let open OTEL.Exporter in
  if es = [] then
    OTEL.Exporter.dummy ()
  else
    {
      emit_spans =
        Emitter_combine.combine_l (List.map (fun e -> e.emit_spans) es);
      emit_logs = Emitter_combine.combine_l (List.map (fun e -> e.emit_logs) es);
      emit_metrics =
        Emitter_combine.combine_l (List.map (fun e -> e.emit_metrics) es);
      on_tick = (fun f -> List.iter (fun e -> e.on_tick f) es);
      tick = (fun () -> List.iter tick es);
      shutdown = (fun ~on_done () -> shutdown_l ~on_done es);
    }

(** [combine exp1 exp2] is the exporter that emits signals to both [exp1] and
    [exp2]. *)
let combine exp1 exp2 : OTEL.Exporter.t = combine_l [ exp1; exp2 ]
