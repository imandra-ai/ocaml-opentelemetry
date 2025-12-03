(** Exporter.

    This is the pluggable component that actually sends signals to a OTEL
    collector, or prints them, or saves them somewhere.

    This is part of the SDK, not just the API, so most real implementations live
    in their own library. *)

open Common_

open struct
  module Proto = Opentelemetry_proto
end

(** Main exporter interface *)
class type t = object
  method send_trace : Proto.Trace.span list -> unit

  method send_metrics : Proto.Metrics.metric list -> unit

  method send_logs : Proto.Logs.log_record list -> unit

  method tick : unit -> unit
  (** Should be called regularly for background processing, timeout checks, etc.
  *)

  method add_on_tick_callback : (unit -> unit) -> unit
  (** Add the given of callback to the exporter when [tick()] is called. The
      callback should be short and reentrant. Depending on the exporter's
      implementation, it might be called from a thread that is not the one that
      called [on_tick]. *)

  method cleanup : on_done:(unit -> unit) -> unit -> unit
  (** [cleanup ~on_done ()] is called when the exporter is shut down, and is
        responsible for sending remaining batches, flushing sockets, etc.
        @param on_done
          callback invoked after the cleanup is done. @since 0.12 *)
end

(** Dummy exporter, does nothing *)
let dummy : t =
  let ticker = Tick_callbacks.create () in
  object
    method send_trace = ignore

    method send_metrics = ignore

    method send_logs = ignore

    method tick () = Tick_callbacks.tick ticker

    method add_on_tick_callback cb = Tick_callbacks.on_tick ticker cb

    method cleanup ~on_done () = on_done ()
  end

let[@inline] send_trace (self : #t) (l : Proto.Trace.span list) =
  self#send_trace l

let[@inline] send_metrics (self : #t) (l : Proto.Metrics.metric list) =
  self#send_metrics l

let[@inline] send_logs (self : #t) (l : Proto.Logs.log_record list) =
  self#send_logs l

let[@inline] on_tick (self : #t) f = self#add_on_tick_callback f

(** Do background work. Call this regularly if the collector doesn't already
    have a ticker thread or internal timer. *)
let[@inline] tick (self : #t) = self#tick ()

let[@inline] cleanup (self : #t) ~on_done : unit = self#cleanup ~on_done ()

(** Main exporter, used by the main tracing functions.

    It is better to pass an explicit exporter when possible. *)
module Main_exporter = struct
  (* hidden *)
  open struct
    (* a list of callbacks automatically added to the main exporter *)
    let on_tick_cbs_ = AList.make ()

    let exporter : t option Atomic.t = Atomic.make None
  end

  (** Set the global exporter *)
  let set (exp : t) : unit =
    List.iter exp#add_on_tick_callback (AList.get on_tick_cbs_);
    Atomic.set exporter (Some exp)

  (** Remove current exporter, if any.
      @param on_done see {!t#cleanup}, @since 0.12 *)
  let remove ~on_done () : unit =
    match Atomic.exchange exporter None with
    | None -> ()
    | Some exp ->
      exp#tick ();
      cleanup exp ~on_done

  (** Is there a configured exporter? *)
  let present () : bool = Option.is_some (Atomic.get exporter)

  (** Current exporter, if any *)
  let[@inline] get () : t option = Atomic.get exporter

  let add_on_tick_callback f =
    AList.add on_tick_cbs_ f;
    Option.iter (fun exp -> exp#add_on_tick_callback f) (get ())
end

let set_backend = Main_exporter.set [@@deprecated "use `Main_exporter.set`"]

let remove_backend = Main_exporter.remove
[@@deprecated "use `Main_exporter.remove`"]

let has_backend = Main_exporter.present
[@@deprecated "use `Main_exporter.present`"]

let get_backend = Main_exporter.get [@@deprecated "use `Main_exporter.ge"]

let with_setup_debug_backend ?(on_done = ignore) (exp : #t) ?(enable = true) ()
    f =
  let exp = (exp :> t) in
  if enable then (
    set_backend exp;
    Fun.protect ~finally:(fun () -> cleanup exp ~on_done) f
  ) else
    f ()
