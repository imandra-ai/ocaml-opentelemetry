open Opentelemetry
open Lwt.Syntax
module Span_id = Span_id
module Trace_id = Trace_id
module Event = Event
module Span = Span
module Span_link = Span_link
module Globals = Globals
module Timestamp_ns = Timestamp_ns
module Gc_metrics = Gc_metrics
module Metrics_callbacks = Metrics_callbacks
module Trace_context = Trace_context
module GC_metrics = Gc_metrics [@@depecated "use Gc_metrics"]
module Metrics_emitter = Metrics_emitter
module Logger = Logger
module Log_record = Log_record

external reraise : exn -> 'a = "%reraise"
(** This is equivalent to [Lwt.reraise]. We inline it here so we don't force to
    use Lwt's latest version *)

module Tracer = struct
  include Tracer

  (** Sync span guard *)
  let with_ (self : t) ?force_new_trace_id ?trace_state ?attrs ?kind ?trace_id
      ?parent ?links name (cb : Span.t -> 'a Lwt.t) : 'a Lwt.t =
    let thunk, finally =
      with_thunk_and_finally self ?force_new_trace_id ?trace_state ?attrs ?kind
        ?trace_id ?parent ?links name cb
    in

    try%lwt
      let* rv = thunk () in
      let () = finally (Ok ()) in
      Lwt.return rv
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      let () = finally (Error (e, bt)) in
      reraise e
end

module Trace = Tracer [@@deprecated "use Tracer"]

module Metrics = struct
  include Metrics
end

module Logs = struct
  include Proto.Logs
  include Log_record
  include Logger
end
