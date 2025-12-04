(** Logs.

    See
    {{:https://opentelemetry.io/docs/reference/specification/overview/#log-signal}
     the spec} *)

open Opentelemetry_emitter

type t = Log_record.t Emitter.t

let dummy : t = Emitter.dummy

let enabled = Emitter.enabled

let of_exporter (exp : Exporter.t) : t = exp.emit_logs

let get_main () : t =
  match Main_exporter.get () with
  | None -> dummy
  | Some e -> e.emit_logs

let emit ?attrs:_ (logs : Log_record.t list) : unit =
  match Main_exporter.get () with
  | None -> ()
  | Some exp -> Exporter.send_logs exp logs
[@@deprecated "use an explicit Logger.t"]

(** An emitter that uses the current {!Main_exporter} *)
let dynamic_forward_to_main_exporter : t =
  Main_exporter.Util.dynamic_forward_to_main_exporter () ~get_emitter:(fun e ->
      e.emit_logs)
