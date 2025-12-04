(** Logs.

    See
    {{:https://opentelemetry.io/docs/reference/specification/overview/#log-signal}
     the spec} *)

open Common_

(** A logger object *)
class type t = object
  method is_enabled : Log_record.severity -> bool

  method emit : Log_record.t list -> unit
end

(** Dummy logger, always disabled *)
let dummy : t =
  object
    method is_enabled _ = false

    method emit _ = ()
  end

class simple (exp : #Exporter.t) : t =
  object
    method is_enabled _ = true

    method emit logs = if logs <> [] then exp#send_logs logs
  end

let emit ?service_name:_ ?attrs:_ (l : Log_record.t list) : unit =
  match Exporter.Main_exporter.get () with
  | None -> ()
  | Some e -> e#send_logs l
[@@deprecated "use an explicit Logger"]

let k_logger : t Context.key = Context.new_key ()
