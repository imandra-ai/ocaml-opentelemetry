open Opentelemetry
open Lwt.Syntax
module Span_id = Span_id
module Trace_id = Trace_id
module Event = Event
module Span = Span
module Span_link = Span_link
module Globals = Globals
module Timestamp_ns = Timestamp_ns
module GC_metrics = GC_metrics
module Metrics_callbacks = Metrics_callbacks
module Trace_context = Trace_context

module Scope = struct
  include Scope

  (**/**)

  let _global_scope_key : t Lwt.key = Lwt.new_key ()

  (**/**)

  let get_surrounding ?scope () : t option =
    let surrounding = Lwt.get _global_scope_key in
    match scope, surrounding with
    | Some _, _ -> scope
    | None, Some _ -> surrounding
    | None, None -> None

  let[@inline] with_scope (sc : t) (f : unit -> 'a) : 'a =
    Lwt.with_value _global_scope_key (Some sc) f
end

open struct
  let get_surrounding_scope = Scope.get_surrounding
end

module Trace = struct
  open Proto.Trace
  include Trace

  (** Sync span guard *)
  let with_ ?(force_new_trace_id = false) ?trace_state ?service_name
      ?(attrs = []) ?kind ?trace_id ?parent ?scope ?links name
      (f : Scope.t -> 'a Lwt.t) : 'a Lwt.t =
    let scope =
      if force_new_trace_id then
        None
      else
        get_surrounding_scope ?scope ()
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
    let scope = { trace_id; span_id; events = []; attrs } in
    (* set global scope in this thread *)
    Scope.with_scope scope @@ fun () ->
    let finally ok =
      let status =
        match ok with
        | Ok () -> default_status ~code:Status_code_ok ()
        | Error e -> default_status ~code:Status_code_error ~message:e ()
      in
      let span, _ =
        Span.create ?kind ~trace_id ?parent ?links ~id:span_id ?trace_state
          ~attrs:scope.attrs ~events:scope.events ~start_time
          ~end_time:(Timestamp_ns.now_unix_ns ())
          ~status name
      in
      emit ?service_name [ span ]
    in
    Lwt.catch
      (fun () ->
        let* x = f scope in
        let () = finally (Ok ()) in
        Lwt.return x)
      (fun e ->
        let () = finally (Error (Printexc.to_string e)) in
        Lwt.fail e)
end

module Metrics = struct
  open Proto.Metrics
  include Metrics
end

module Logs = struct
  include Proto.Logs
  include Logs
end
