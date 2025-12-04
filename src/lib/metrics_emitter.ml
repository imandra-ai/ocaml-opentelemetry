open Opentelemetry_emitter

type t = Metrics.t Emitter.t

let dummy () : t = Emitter.dummy ()

let enabled = Emitter.enabled

let emit = Emitter.emit

let of_exporter (exp : Exporter.t) : t = exp.emit_metrics

(** Emit some metrics to the collector (sync). This blocks until the backend has
    pushed the metrics into some internal queue, or discarded them. *)
let emit ?attrs:_ (l : Metrics.t list) : unit =
  match Exporter.Main_exporter.get () with
  | None -> ()
  | Some exp -> Exporter.send_metrics exp l
[@@deprecated "use an explicit Metrics_emitter.t"]
