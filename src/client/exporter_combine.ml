open Common_
open Opentelemetry_atomic

open struct
  let shutdown_l (es : OTEL.Exporter.t list) ~trigger : unit =
    let missing = Atomic.make (List.length es) in
    let on_done () =
      if Atomic.fetch_and_add missing (-1) = 1 then
        (* we were the last exporter to shutdown, [missing] is now 0 *)
        Aswitch.turn_off trigger
    in

    List.iter (fun e -> Aswitch.on_turn_off (OTEL.Exporter.active e) on_done) es;
    List.iter OTEL.Exporter.shutdown es
end

let combine_l (es : OTEL.Exporter.t list) : OTEL.Exporter.t =
  let open OTEL.Exporter in
  if es = [] then
    OTEL.Exporter.dummy ()
  else (
    let active, trigger = Aswitch.create () in
    {
      active = (fun () -> active);
      emit_spans =
        Emitter_combine.combine_l (List.map (fun e -> e.emit_spans) es);
      emit_logs = Emitter_combine.combine_l (List.map (fun e -> e.emit_logs) es);
      emit_metrics =
        Emitter_combine.combine_l (List.map (fun e -> e.emit_metrics) es);
      on_tick = (fun f -> List.iter (fun e -> e.on_tick f) es);
      tick = (fun () -> List.iter tick es);
      shutdown = (fun () -> shutdown_l es ~trigger);
      self_metrics =
        (fun () -> List.fold_left (fun acc e -> e.self_metrics () @ acc) [] es);
    }
  )

(** [combine exp1 exp2] is the exporter that emits signals to both [exp1] and
    [exp2]. *)
let combine exp1 exp2 : OTEL.Exporter.t = combine_l [ exp1; exp2 ]
