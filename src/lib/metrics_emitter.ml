open Opentelemetry_emitter

type t = Metrics.t Emitter.t

let dummy () : t = Emitter.dummy ()

let enabled = Emitter.enabled

let of_exporter (exp : Exporter.t) : t = exp.emit_metrics

(** Emit some metrics to the collector (sync). This blocks until the backend has
    pushed the metrics into some internal queue, or discarded them. *)
let (emit [@deprecated "use an explicit Metrics_emitter.t"]) =
 fun ?attrs:_ (l : Metrics.t list) : unit ->
  match Main_exporter.get () with
  | None -> ()
  | Some exp -> Exporter.send_metrics exp l

(** An emitter that uses the current {!Main_exporter} *)
let dynamic_forward_to_main_exporter : t =
  Main_exporter.Util.dynamic_forward_to_main_exporter () ~get_emitter:(fun e ->
      e.emit_metrics)
