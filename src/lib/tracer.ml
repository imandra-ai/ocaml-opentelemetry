(** Traces.

    See
    {{:https://opentelemetry.io/docs/reference/specification/overview/#tracing-signal}
     the spec} *)

open Common_
open Proto.Trace
open Opentelemetry_emitter

type span = Span.t

type t = Span.t Emitter.t
(** A tracer.

    https://opentelemetry.io/docs/specs/otel/trace/api/#tracer *)

(** Dummy tracer, always disabled *)
let dummy () : t = Emitter.dummy ()

(** A tracer that uses the current {!Main_exporter} *)
let dynamic_forward_to_main_exporter : t =
  Main_exporter.Util.dynamic_forward_to_main_exporter () ~get_emitter:(fun e ->
      e.emit_spans)

let (add_event [@deprecated "use Span.add_event"]) = Span.add_event

let (add_attrs [@deprecated "use Span.add_attrs"]) = Span.add_attrs

let with_thunk_and_finally (self : t) ?(force_new_trace_id = false) ?trace_state
    ?(attrs : (string * [< Value.t ]) list = []) ?kind ?trace_id ?parent ?links
    name cb =
  let parent =
    match parent with
    | Some _ -> parent
    | None -> Ambient_span.get ()
  in
  let trace_id =
    match trace_id, parent with
    | _ when force_new_trace_id -> Trace_id.create ()
    | Some trace_id, _ -> trace_id
    | None, Some p -> Span.trace_id p
    | None, None -> Trace_id.create ()
  in
  let start_time = Timestamp_ns.now_unix_ns () in
  let span_id = Span_id.create () in

  let parent_id = Option.map Span.id parent in

  let span : Span.t =
    Span.make ?trace_state ?kind ?parent:parent_id ~trace_id ~id:span_id ~attrs
      ?links ~start_time ~end_time:start_time name
  in
  (* called once we're done, to emit a span *)
  let finally res =
    let end_time = Timestamp_ns.now_unix_ns () in
    Proto.Trace.span_set_end_time_unix_nano span end_time;

    (match Span.status span with
    | Some _ -> ()
    | None ->
      (match res with
      | Ok () ->
        (* By default, all spans are Unset, which means a span completed without error.
             The Ok status is reserved for when you need to explicitly mark a span as successful
             rather than stick with the default of Unset (i.e., “without error”).

              https://opentelemetry.io/docs/languages/go/instrumentation/#set-span-status *)
        ()
      | Error (e, bt) ->
        Span.record_exception span e bt;
        let status =
          make_status ~code:Status_code_error ~message:(Printexc.to_string e) ()
        in
        Span.set_status span status));

    Emitter.emit self [ span ]
  in
  let thunk () = Ambient_span.with_ambient span (fun () -> cb span) in
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
let with_ (self : t) ?force_new_trace_id ?trace_state ?attrs ?kind ?trace_id
    ?parent ?links name (cb : Span.t -> 'a) : 'a =
  let thunk, finally =
    with_thunk_and_finally self ?force_new_trace_id ?trace_state ?attrs ?kind
      ?trace_id ?parent ?links name cb
  in

  try
    let rv = thunk () in
    finally (Ok ());
    rv
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    finally (Error (e, bt));
    raise e
