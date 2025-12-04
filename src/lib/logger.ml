(** Logs.

    See
    {{:https://opentelemetry.io/docs/reference/specification/overview/#log-signal}
     the spec} *)

open Opentelemetry_emitter

type t = Log_record.t Emitter.t

let dummy () : t = Emitter.dummy ()

let enabled = Emitter.enabled

let emit = Emitter.emit

let of_exporter (exp : Exporter.t) : t = exp.emit_logs
