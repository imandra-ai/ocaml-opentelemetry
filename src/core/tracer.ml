(** Traces.

    See
    {{:https://opentelemetry.io/docs/reference/specification/overview/#tracing-signal}
     the spec} *)

open Common_
open Proto.Trace

type span = Span.t

(** A tracer.

    https://opentelemetry.io/docs/specs/otel/trace/api/#tracer *)
class type t = object
  method is_enabled : unit -> bool

  method emit : span list -> unit
end

(** Dummy tracer, always disabled *)
let dummy : t =
  object
    method is_enabled () = false

    method emit _ = ()
  end

(** A simple exporter that directly calls the exporter. *)
class simple (exp : #Exporter.t) : t =
  object
    method is_enabled () = true

    method emit spans = if spans <> [] then Exporter.send_trace exp spans
  end

(** A tracer that uses {!Exporter.Main_exporter} *)
let simple_main_exporter : t =
  object
    method is_enabled () = Exporter.Main_exporter.present ()

    method emit spans =
      match Exporter.Main_exporter.get () with
      | None -> ()
      | Some exp -> exp#send_trace spans
  end

(** Directly emit to the main exporter.

    {b NOTE} be careful not to call this inside a Gc alarm, as it can cause
    deadlocks. *)
let emit ?service_name:_ ?attrs:_ (spans : span list) : unit =
  match Exporter.Main_exporter.get () with
  | None -> ()
  | Some exp -> exp#send_trace spans
[@@deprecated "use an explicit tracer"]

(* TODO: remove scope, use span directly *)
type scope = Scope.t = {
  trace_id: Trace_id.t;
  span_id: Span_id.t;
  mutable items: Scope.item_list;
}
[@@deprecated "use Scope.t"]

let (add_event [@deprecated "use Scope.add_event"]) = Scope.add_event

let (add_attrs [@deprecated "use Scope.add_attrs"]) = Scope.add_attrs

let with_' ?(tracer = simple_main_exporter) ?(force_new_trace_id = false)
    ?trace_state ?(attrs : (string * [< Value.t ]) list = []) ?kind ?trace_id
    ?parent ?scope ?(links = []) name cb =
  let scope =
    if force_new_trace_id then
      None
    else
      Scope.get_ambient_scope ?scope ()
  in
  let trace_id =
    match trace_id, scope with
    | _ when force_new_trace_id -> Trace_id.create ()
    | Some trace_id, _ -> trace_id
    | None, Some scope -> scope.trace_id
    | None, None -> Trace_id.create ()
  in
  let parent =
    match parent, scope with
    | _ when force_new_trace_id -> None
    | Some span_id, _ -> Some span_id
    | None, Some scope -> Some scope.span_id
    | None, None -> None
  in
  let start_time = Timestamp_ns.now_unix_ns () in
  let span_id = Span_id.create () in
  let scope = Scope.make ~trace_id ~span_id ~attrs ~links () in
  (* called once we're done, to emit a span *)
  let finally res =
    let status =
      match Scope.status scope with
      | Some status -> Some status
      | None ->
        (match res with
        | Ok () ->
          (* By default, all spans are Unset, which means a span completed without error.
             The Ok status is reserved for when you need to explicitly mark a span as successful
             rather than stick with the default of Unset (i.e., “without error”).

              https://opentelemetry.io/docs/languages/go/instrumentation/#set-span-status *)
          None
        | Error (e, bt) ->
          Scope.record_exception scope e bt;
          Some
            (make_status ~code:Status_code_error ~message:(Printexc.to_string e)
               ()))
    in
    let span, _ =
      (* TODO: should the attrs passed to with_ go on the Span
           (in Span.create) or on the ResourceSpan (in emit)?
           (question also applies to Opentelemetry_lwt.Trace.with) *)
      Span.create ?kind ~trace_id ?parent ~links:(Scope.links scope) ~id:span_id
        ?trace_state ~attrs:(Scope.attrs scope) ~events:(Scope.events scope)
        ~start_time
        ~end_time:(Timestamp_ns.now_unix_ns ())
        ?status name
    in

    tracer#emit [ span ]
  in
  let thunk () =
    (* set global scope in this thread *)
    Scope.with_ambient_scope scope @@ fun () -> cb scope
  in
  thunk, finally

(** Sync span guard.

    Notably, this includes {e implicit} scope-tracking: if called without a
    [~scope] argument (or [~parent]/[~trace_id]), it will check in the
    {!Ambient_context} for a surrounding environment, and use that as the scope.
    Similarly, it uses {!Scope.with_ambient_scope} to {e set} a new scope in the
    ambient context, so that any logically-nested calls to {!with_} will use
    this span as their parent.

    {b NOTE} be careful not to call this inside a Gc alarm, as it can cause
    deadlocks.

    @param force_new_trace_id
      if true (default false), the span will not use a ambient scope, the
      [~scope] argument, nor [~trace_id], but will instead always create fresh
      identifiers for this span *)
let with_ ?tracer ?force_new_trace_id ?trace_state ?attrs ?kind ?trace_id
    ?parent ?scope ?links name (cb : Scope.t -> 'a) : 'a =
  let thunk, finally =
    with_' ?tracer ?force_new_trace_id ?trace_state ?attrs ?kind ?trace_id
      ?parent ?scope ?links name cb
  in

  try
    let rv = thunk () in
    finally (Ok ());
    rv
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    finally (Error (e, bt));
    raise e
