open Opentelemetry
open Lwt.Syntax

module Span_id = Span_id
module Trace_id = Trace_id
module Span = Span
module Globals = Globals
module Timestamp_ns = Timestamp_ns

module Trace = struct
  open Proto.Trace
  include Trace

  (** Emit asynchronously *)
  let emit (spans:span list) : unit Lwt.t =
    let fut, wake = Lwt.wait() in
    let ils =
      default_instrumentation_library_spans ~spans () in
    let rs = default_resource_spans ~instrumentation_library_spans:[ils] () in
    Collector.send_trace [rs]
      ~over:(fun () -> Lwt.wakeup_later wake ())
      ~ret:(fun () -> fut)

  (** Sync span guard *)
  let with_
      ?trace_state ?service_name ?attrs
      ?kind ?(trace_id=Trace_id.create()) ?parent ?links
      name (f:Trace_id.t * Span_id.t -> 'a Lwt.t) : 'a Lwt.t =
    let start_time = Timestamp_ns.now_unix_ns() in
    let span_id = Span_id.create() in
    let finally ok =
      let status = match ok with
        | Ok () -> default_status ~code:Status_code_ok ()
        | Error e -> default_status ~code:Status_code_error ~message:e () in
      let span, _ =
        Span.create
          ?kind ~trace_id ?parent ?links ~id:span_id
          ?trace_state ?service_name ?attrs
          ~start_time ~end_time:(Timestamp_ns.now_unix_ns())
          ~status
          name in
      emit [span]
    in
    Lwt.catch
      (fun () ->
         let* x = f (trace_id,span_id) in
         let+ () = finally (Ok ()) in
         x)
      (fun e ->
         let* () = finally (Error (Printexc.to_string e)) in
         Lwt.fail e)
end

module Metrics = struct
  open Proto.Metrics
  include Metrics

  (** Emit some metrics to the collector. *)
  let emit (l:t list) : unit Lwt.t =
    let fut, wake = Lwt.wait() in
    let lm =
      default_instrumentation_library_metrics ~metrics:l () in
    let rm = default_resource_metrics
        ~instrumentation_library_metrics:[lm] () in
    Collector.send_metrics [rm]
      ~over:(fun () -> Lwt.wakeup_later wake ())
      ~ret:(fun () -> fut)
end
