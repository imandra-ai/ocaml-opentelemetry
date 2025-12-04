(** Emitters *)

exception Closed

type 'a t = {
  emit: 'a list -> unit;
      (** Emit signals. @raise Closed if the emitter is closed. *)
  tick: now:Mtime.t -> unit;
      (** Call regularly to ensure background work is done *)
  closed: unit -> bool;  (** True if the emitter was closed *)
  flush_and_close: unit -> unit;
      (** Flush internal buffered signals, then close *)
}
(** An emitter for values of type ['a]. *)

let[@inline] emit (self : _ t) l : unit = if l <> [] then self.emit l

let[@inline] tick (self : _ t) ~now : unit = self.tick ~now

let[@inline] closed self : bool = self.closed ()

let[@inline] flush_and_close (self : _ t) : unit = self.flush_and_close ()

let map (f : 'a -> 'b) (self : 'b t) : 'a t =
  { self with emit = (fun l -> self.emit (List.map f l)) }

(* TODO: batching, either regular or sharded to reduce contention *)
(* TODO: sampling *)

(* TODO: use in Opentelemetry, and also for Tracer, Logger, etc. *)
