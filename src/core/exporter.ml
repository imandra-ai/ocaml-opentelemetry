(** Exporter.

    This is the pluggable component that actually sends signals to a OTEL
    collector, or prints them, or saves them somewhere.

    This is part of the SDK, not just the API, so most real implementations live
    in their own library. *)

open Common_
open Opentelemetry_emitter

open struct
  module Proto = Opentelemetry_proto
end

type t = {
  emit_spans: Proto.Trace.span Emitter.t;
  emit_metrics: Proto.Metrics.metric Emitter.t;
  emit_logs: Proto.Logs.log_record Emitter.t;
  on_tick: Cb_set.t;
      (** Set of callbacks for "on tick". Should be triggered regularly for
          background processing, timeout checks, etc. *)
  cleanup: on_done:(unit -> unit) -> unit -> unit;
      (** [cleanup ~on_done ()] is called when the exporter is shut down, and is
        responsible for sending remaining batches, flushing sockets, etc.
        @param on_done
          callback invoked after the cleanup is done. @since 0.12 *)
}
(** Main exporter interface. *)

(** Dummy exporter, does nothing *)
let dummy () : t =
  let on_tick = Cb_set.create () in
  {
    emit_spans = Emitter.dummy ();
    emit_metrics = Emitter.dummy ();
    emit_logs = Emitter.dummy ();
    on_tick;
    cleanup = (fun ~on_done () -> on_done ());
  }

let[@inline] send_trace (self : t) (l : Proto.Trace.span list) =
  Emitter.emit self.emit_spans l

let[@inline] send_metrics (self : t) (l : Proto.Metrics.metric list) =
  Emitter.emit self.emit_metrics l

let[@inline] send_logs (self : t) (l : Proto.Logs.log_record list) =
  Emitter.emit self.emit_logs l

let on_tick (self : t) f = Cb_set.register self.on_tick f

(** Do background work. Call this regularly if the collector doesn't already
    have a ticker thread or internal timer. *)
let tick (self : t) =
  Cb_set.trigger self.on_tick;
  (* also tick each emitter! *)
  let now = Mtime_clock.now () in
  Emitter.tick ~now self.emit_spans;
  Emitter.tick ~now self.emit_metrics;
  Emitter.tick ~now self.emit_logs;
  ()

let[@inline] cleanup (self : t) ~on_done : unit = self.cleanup ~on_done ()

(** Main exporter, used by the main tracing functions.

    It is better to pass an explicit exporter when possible. *)
module Main_exporter = struct
  (* hidden *)
  open struct
    (* a list of callbacks automatically added to the main exporter *)
    let on_tick_cbs_ = Alist.make ()

    let exporter : t option Atomic.t = Atomic.make None
  end

  (** Set the global exporter *)
  let set (exp : t) : unit =
    List.iter (on_tick exp) (Alist.get on_tick_cbs_);
    Atomic.set exporter (Some exp)

  (** Remove current exporter, if any.
      @param on_done see {!t#cleanup}, @since 0.12 *)
  let remove ~on_done () : unit =
    match Atomic.exchange exporter None with
    | None -> ()
    | Some exp ->
      tick exp;
      cleanup exp ~on_done

  (** Is there a configured exporter? *)
  let present () : bool = Option.is_some (Atomic.get exporter)

  (** Current exporter, if any *)
  let[@inline] get () : t option = Atomic.get exporter

  let add_on_tick_callback f =
    Alist.add on_tick_cbs_ f;
    Option.iter (fun exp -> on_tick exp f) (get ())
end

let (set_backend [@deprecated "use `Main_exporter.set`"]) = Main_exporter.set

let (remove_backend [@deprecated "use `Main_exporter.remove`"]) =
  Main_exporter.remove

let (has_backend [@deprecated "use `Main_exporter.present`"]) =
  Main_exporter.present

let (get_backend [@deprecated "use `Main_exporter.ge"]) = Main_exporter.get

let with_setup_debug_backend ?(on_done = ignore) (exp : t) ?(enable = true) () f
    =
  if enable then (
    Main_exporter.set exp;
    Fun.protect ~finally:(fun () -> cleanup exp ~on_done) f
  ) else
    f ()
