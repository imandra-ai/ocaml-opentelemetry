(* A runs tests against a OTel-instrumented program  *)

module Client = Opentelemetry_client
module Signal = Client.Signal
module Proto = Opentelemetry.Proto
open Lwt.Syntax

(* Server to collect telemetry data *)
module Server = struct
  let dbg_request kind req pp data : unit Lwt.t =
    let _ = kind, req, pp, data in
    (* NOTE: Uncomment for debugging *)
    (* let* () = *)
    (*   let req : string = Format.asprintf "%a" Http.Request.pp req in *)
    (*   let data_s : string = Format.asprintf "%a" pp data in *)
    (*   Lwt_io.fprintf Lwt_io.stderr "# received %s\nREQUEST: %s\nBODY: %s\n@." *)
    (*     kind req data_s *)
    (* in *)
    Lwt.return ()

  let metrics req data =
    let metrics = Signal.Decode.metrics data in
    let+ () = dbg_request "metrics" req Signal.Pp.metrics metrics in
    Signal.Metrics metrics

  let handler push_signal _socket (request : Http.Request.t)
      (body : Cohttp_lwt.Body.t) =
    let* data = Cohttp_lwt.Body.to_string body in
    let* status, signal =
      match Http.Request.resource request with
      | "/v1/traces" ->
        let traces = Signal.Decode.traces data in
        let+ () = dbg_request "trace" request Signal.Pp.traces traces in
        `OK, Some (Signal.Traces traces)
      | "/v1/metrics" ->
        let metrics = Signal.Decode.metrics data in
        let+ () = dbg_request "metrics" request Signal.Pp.metrics metrics in
        `OK, Some (Signal.Metrics metrics)
      | "/v1/logs" ->
        let logs = Signal.Decode.logs data in
        let+ () = dbg_request "logs" request Signal.Pp.logs logs in
        `OK, Some (Signal.Logs logs)
      | unexepected ->
        let+ () = Lwt_io.eprintf "unexpected endpoint %s\n" unexepected in
        `Not_found, None
    in
    push_signal signal;
    let resp_body = Cohttp_lwt.Body.of_string "" in
    Cohttp_lwt_unix.Server.respond ~status ~body:resp_body ()

  let run port push_signals =
    let* () = Lwt_io.eprintf "starting server\n" in
    Cohttp_lwt_unix.Server.(
      make ~callback:(handler push_signals) ()
      |> create ~mode:(`TCP (`Port port)))
end

(** Manage launching and cleaning up the program we are testing *)
module Tested_program = struct
  let validate_exit = function
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED bad_code ->
      failwith
      @@ Printf.sprintf "process under test ended with bad exit code %d"
           bad_code
    | Unix.WSIGNALED i ->
      failwith
      @@ Printf.sprintf "process under test ended with unexpected signal %d" i
    | Unix.WSTOPPED i ->
      failwith
      @@ Printf.sprintf "process under test ended with unexpected stop %d" i

  let run program_to_test =
    let redirect = `FD_copy Unix.stderr in
    let cmd = "", Array.of_list program_to_test in
    (* Give server time to be online *)
    let* () = Lwt_unix.sleep 0.5 in
    let* () =
      Lwt_io.eprintf "running command: %s\n"
        (Format.asprintf "%a"
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.pp_print_string fmt " ")
              Format.pp_print_string)
           program_to_test)
    in
    let* result = Lwt_process.exec ~stdout:redirect cmd in
    (* Give server time process signals *)
    let+ () = Lwt_unix.sleep 0.5 in
    validate_exit result
end

let collect_traces ~port program_to_test push_signals () =
  let* () =
    Lwt.pick
      [ Server.run port push_signals; Tested_program.run program_to_test ]
  in
  (* Let the tester know all the signals have be sent *)
  Lwt.return (push_signals None)

let normalize_scope_span : Proto.Trace.scope_spans -> Proto.Trace.scope_spans =
  function
  | scope_span ->
    {
      scope_span with
      spans =
        scope_span.spans
        |> List.map (fun (span : Proto.Trace.span) ->
               {
                 span with
                 start_time_unix_nano = -1L;
                 end_time_unix_nano = -1L;
               });
    }

let normalize_signal : Signal.t -> Signal.t = function
  | Traces ts ->
    Traces
      (ts
      |> List.map (fun (trace : Proto.Trace.resource_spans) ->
             {
               trace with
               scope_spans = trace.scope_spans |> List.map normalize_scope_span;
             }))
  | x -> x

(* normalize trace output by redacting non-deterministic values from output *)
let normalize =
  let re =
    Str.regexp
      {|\(start_time_unix_nano\|time_unix_nano\|end_time_unix_nano\|value\) = \([0-9]*\|As_int([0-9]*)\|As_double([0-9]*\.)\);|}
  in
  fun s -> Str.global_replace re {|\1 = <redacted>;|} s

let default_port =
  String.split_on_char ':' Client.Config.default_url |> function
  (* Extracting the port from 'http://foo:<port>' *)
  | [ _; _; port ] -> int_of_string port
  | _ -> failwith "unexpected format in Client.Config.default_url"

let gather_signals ?(port = default_port) program_to_test =
  Lwt_main.run
  @@
  let stream, push = Lwt_stream.create () in
  let* () = collect_traces ~port program_to_test push () in
  Lwt_stream.to_list stream

let run ?(port = default_port) ~program_to_test () =
  gather_signals ~port program_to_test
  |> List.map (fun s -> s |> Format.asprintf "%a" Signal.Pp.pp |> normalize)
  |> List.stable_sort String.compare (* Produce a deterministic order *)
  |> List.iter print_string
