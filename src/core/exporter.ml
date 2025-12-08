(** Exporter.

    This is the pluggable component that actually sends signals to a OTEL
    collector, or prints them, or saves them somewhere.

    This is part of the SDK, not just the API, so most real implementations live
    in their own library. *)

open Common_
open Opentelemetry_emitter

type t = {
  emit_spans: Proto.Trace.span Emitter.t;
  emit_metrics: Proto.Metrics.metric Emitter.t;
  emit_logs: Proto.Logs.log_record Emitter.t;
  on_tick: (unit -> unit) -> unit;
  tick: unit -> unit;
      (** Call all the callbacks registered with [on_tick]. Should be triggered
          regularly for background processing, timeout checks, etc. *)
  shutdown: on_done:(unit -> unit) -> unit -> unit;
      (** [shutdown ~on_done ()] is called when the exporter is shut down, and is
        responsible for sending remaining batches, flushing sockets, etc.
        @param on_done
          callback invoked after the shutdown is done. @since 0.12 *)
}
(** Main exporter interface. *)

(** Dummy exporter, does nothing *)
let dummy () : t =
  let ticker = Cb_set.create () in
  {
    emit_spans = Emitter.dummy;
    emit_metrics = Emitter.dummy;
    emit_logs = Emitter.dummy;
    on_tick = Cb_set.register ticker;
    tick = (fun () -> Cb_set.trigger ticker);
    shutdown = (fun ~on_done () -> on_done ());
  }

let[@inline] send_trace (self : t) (l : Proto.Trace.span list) =
  Emitter.emit self.emit_spans l

let[@inline] send_metrics (self : t) (l : Proto.Metrics.metric list) =
  Emitter.emit self.emit_metrics l

let[@inline] send_logs (self : t) (l : Proto.Logs.log_record list) =
  Emitter.emit self.emit_logs l

let[@inline] on_tick (self : t) f = self.on_tick f

(** Do background work. Call this regularly if the collector doesn't already
    have a ticker thread or internal timer. *)
let tick (self : t) =
  (* make sure emitters get the chance to check timeouts, flush, etc. *)
  let now = Mtime_clock.now () in
  Emitter.tick ~now self.emit_spans;
  Emitter.tick ~now self.emit_metrics;
  Emitter.tick ~now self.emit_logs;

  (* call the callbacks *)
  self.tick ();
  ()

let[@inline] shutdown (self : t) ~on_done : unit = self.shutdown ~on_done ()

let (cleanup [@deprecated "use shutdown instead"]) = shutdown
