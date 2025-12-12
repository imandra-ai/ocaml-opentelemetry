(** Main exporter, used by the main tracing functions.

    It is better to pass an explicit exporter when possible. *)

open Exporter

(* hidden *)
open struct
  (* a list of callbacks automatically added to the main exporter *)
  let on_tick_cbs_ = Alist.make ()

  let exporter : t option Atomic.t = Atomic.make None
end

(** Remove current exporter, if any.
    @param on_done
      called when this operation is done, including shutting down the exporter,
      if any *)
let remove ~on_done () : unit =
  match Atomic.exchange exporter None with
  | None -> on_done ()
  | Some exp ->
    Aswitch.on_turn_off (Exporter.active exp) on_done;
    tick exp;
    shutdown exp

(** Is there a configured exporter? *)
let present () : bool = Option.is_some (Atomic.get exporter)

(** Current exporter, if any *)
let[@inline] get () : t option = Atomic.get exporter

let add_on_tick_callback f =
  Alist.add on_tick_cbs_ f;
  Option.iter (fun exp -> on_tick exp f) (get ())

module Util = struct
  open Opentelemetry_emitter

  (** An emitter that uses the current main *)
  let dynamic_forward_to_main_exporter ~get_emitter () : _ Emitter.t =
    let enabled () = present () in
    let closed () = not (enabled ()) in
    let flush_and_close () = () in
    let tick ~now:_ =
      match get () with
      | None -> ()
      | Some exp -> Exporter.tick exp
    in
    let emit signals =
      if signals <> [] then (
        match get () with
        | None -> ()
        | Some exp ->
          let emitter = get_emitter exp in
          Emitter.emit emitter signals
      )
    in
    { Emitter.enabled; closed; emit; tick; flush_and_close }
end

(** Aswitch of the current exporter, or {!Aswitch.dummy} *)
let[@inline] active () : Aswitch.t =
  match get () with
  | None -> Aswitch.dummy
  | Some e -> e.active ()

(** This exporter uses the current "main exporter" using [get()] at every
    invocation. It is useful as a fallback or to port existing applications that
    expect a global singleton backend^W exporter.
    @since NEXT_RELEASE *)
let dynamic_forward_to_main_exporter : Exporter.t =
  let emit_logs =
    Util.dynamic_forward_to_main_exporter ()
      ~get_emitter:Exporter.(fun e -> e.emit_logs)
  in
  let emit_metrics =
    Util.dynamic_forward_to_main_exporter ()
      ~get_emitter:Exporter.(fun e -> e.emit_metrics)
  in
  let emit_spans =
    Util.dynamic_forward_to_main_exporter ()
      ~get_emitter:Exporter.(fun e -> e.emit_spans)
  in
  let on_tick f =
    match get () with
    | None -> ()
    | Some exp -> Exporter.on_tick exp f
  in
  let tick () =
    match get () with
    | None -> ()
    | Some exp -> exp.tick ()
  in
  let self_metrics () =
    match get () with
    | None -> []
    | Some exp -> exp.self_metrics ()
  in
  let shutdown () = () in
  {
    Exporter.active;
    emit_metrics;
    emit_spans;
    emit_logs;
    on_tick;
    tick;
    shutdown;
    self_metrics;
  }

let self_metrics () : Metrics.t list =
  dynamic_forward_to_main_exporter.self_metrics ()

(** Set the global exporter *)
let set (exp : t) : unit =
  (* sanity check! this specific exporter would just call itself, leading to
     stack overflow. *)
  if exp == dynamic_forward_to_main_exporter then
    failwith
      "cannot set Main_exporter.dynamic_forward_to_main_exporter as main \
       exporter!";

  List.iter (on_tick exp) (Alist.get on_tick_cbs_);
  Atomic.set exporter (Some exp)

let (set_backend [@deprecated "use `Main_exporter.set`"]) = set

let (remove_backend [@deprecated "use `Main_exporter.remove`"]) = remove

let (has_backend [@deprecated "use `Main_exporter.present`"]) = present

let (get_backend [@deprecated "use `Main_exporter.get"]) = get

let with_setup_debug_backend ?(on_done = ignore) (exp : t) ?(enable = true) () f
    =
  if enable then (
    set exp;
    Aswitch.on_turn_off (Exporter.active exp) on_done;
    Fun.protect f ~finally:(fun () -> Exporter.shutdown exp)
  ) else
    Fun.protect f ~finally:(fun () -> on_done ())
