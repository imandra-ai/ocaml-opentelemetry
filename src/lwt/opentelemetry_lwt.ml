open Lwt.Syntax
include Opentelemetry

module Main_exporter = struct
  include Main_exporter

  let remove () : unit Lwt.t =
    let p, resolve = Lwt.wait () in
    Aswitch.on_turn_off (active ()) (fun () -> Lwt.wakeup_later resolve ());
    p
end

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
