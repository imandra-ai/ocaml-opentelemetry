open Common_

class type t = object
  method is_enabled : unit -> bool

  method emit : Metrics.t list -> unit
end

class dummy : t =
  object
    method is_enabled () = false

    method emit _ = ()
  end

class simple (exp : #Exporter.t) : t =
  object
    method is_enabled () = true

    method emit l = if l <> [] then exp#send_metrics l
  end

(** Emit some metrics to the collector (sync). This blocks until the backend has
    pushed the metrics into some internal queue, or discarded them.

    {b NOTE} be careful not to call this inside a Gc alarm, as it can cause
    deadlocks. *)
let emit ?attrs:_ (l : Metrics.t list) : unit =
  match Exporter.Main_exporter.get () with
  | None -> ()
  | Some exp -> exp#send_metrics l
[@@deprecated "use an explicit Metrics_emitter.t"]
