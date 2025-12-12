(** Emitters.

    This is the composable abstraction we use to represent how signals are
    emitted, from their origin point (a site in user code or library code that
    was instrumented, and just created a span or log record or metric), down to
    the actual SDK exporter installed in the application. *)

exception Closed

type -'a t = {
  enabled: unit -> bool;
      (** Return [true] if [emit] has a chance of doing something with the
          signals it's given. *)
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

let[@inline] enabled self : bool = self.enabled ()

let[@inline] emit (self : _ t) l : unit = if l <> [] then self.emit l

let[@inline] tick (self : _ t) ~now : unit = self.tick ~now

let[@inline] closed self : bool = self.closed ()

let[@inline] flush_and_close (self : _ t) : unit = self.flush_and_close ()

(** [map f emitter] returns a new emitter that applies [f] to signals item-wise
    before passing them to [emitter] *)
let map (f : 'a -> 'b) (self : 'b t) : 'a t =
  { self with emit = (fun l -> self.emit (List.map f l)) }

(** [map_l f emitter] applies [f] to incoming lists of signals, and emits the
    resulting list (if non empty) *)
let flat_map (f : 'a list -> 'b list) (self : 'b t) : 'a t =
  let emit l =
    match f l with
    | [] -> ()
    | fl -> self.emit fl
  in
  { self with emit }

(** [tap f e] is like [e], but every signal is passed to [f] *)
let tap (f : 'a -> unit) (self : 'a t) : 'a t =
  let emit l =
    List.iter f l;
    self.emit l
  in
  { self with emit }

(** [make_simple ~emit ()] is an emitter that calls [emit]. *)
let make_simple ?tick ?closed ?enabled ?(flush_and_close = ignore) ~emit () :
    _ t =
  let tick =
    match tick with
    | None -> fun ~now:_ -> ()
    | Some f -> f
  in
  let closed, enabled =
    match closed, enabled with
    | None, None -> (fun () -> false), fun () -> true
    | Some f, None -> f, fun () -> not (f ())
    | None, Some f -> (fun () -> not (f ())), f
    | Some f1, Some f2 -> f1, f2
  in
  { tick; emit; flush_and_close; closed; enabled }

(** Dummy emitter, doesn't accept or emit anything. *)
let dummy : _ t =
  {
    enabled = (fun () -> false);
    emit = ignore;
    tick = (fun ~now:_ -> ());
    closed = (fun () -> true);
    flush_and_close = ignore;
  }
