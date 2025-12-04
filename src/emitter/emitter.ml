(** Emitters.

    This is the composable abstraction we use to represent how signals are
    emitted, from their origin point (a site in user code or library code that
    was instrumented, and just created a span or log record or metric), down to
    the actual SDK exporter installed in the application. *)

exception Closed

type 'a t = {
  emit: 'a list -> unit;
      (** Emit signals. @raise Closed if the emitter is closed. *)
  tick: now:Mtime.t -> unit;
      (** Call regularly to ensure background work is done. The current
          timestamp is passed to improve testability. *)
  closed: unit -> bool;
      (** True if the emitter is already closed. Beware TOCTOU bugs. *)
  flush_and_close: unit -> unit;
      (** Flush internally buffered signals, then close. *)
}
(** An emitter for values of type ['a]. *)

let[@inline] emit (self : _ t) l : unit = if l <> [] then self.emit l

let[@inline] tick (self : _ t) ~now : unit = self.tick ~now

let[@inline] closed self : bool = self.closed ()

let[@inline] flush_and_close (self : _ t) : unit = self.flush_and_close ()

(** [map f emitter] returns a new emitter that applies [f] to signals before
    passing them to [emitter] *)
let map (f : 'a -> 'b) (self : 'b t) : 'a t =
  { self with emit = (fun l -> self.emit (List.map f l)) }

(* TODO: batching, either regular or sharded to reduce contention *)
(* TODO: sampling *)

(* TODO: use in Opentelemetry, and also for Tracer, Logger, etc. *)
