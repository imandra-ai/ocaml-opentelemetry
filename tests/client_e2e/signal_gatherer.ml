(* A runs tests against a OTel-instrumented program  *)

module Client = Opentelemetry_client
module Signal = Client.Signal
open Lwt.Syntax

let debug =
  match Sys.getenv_opt "DEBUG" with
  | Some "1" -> true
  | _ -> false

(* Server to collect telemetry data *)
module Server = struct
  let dbg_request kind req pp data : unit Lwt.t =
    if debug then (
      let _ = kind, req, pp, data in
      let req : string = Format.asprintf "%a" Http.Request.pp req in
      let data_s : string = Format.asprintf "%a" pp data in
      Lwt_io.fprintf Lwt_io.stderr "# received %s\nREQUEST: %s\nBODY: %s\n@."
        kind req data_s
    ) else
      Lwt.return_unit

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
        let+ () =
          Lwt_io.eprintf "unexpected endpoint %s -- status %s\n" unexepected
            (Http.Status.to_string `Not_found)
        in
        `Not_found, None
    in
    push_signal signal;
    let resp_body = Cohttp_lwt.Body.of_string "" in
    Cohttp_lwt_unix.Server.respond ~status ~body:resp_body ()

  let run port push_signals =
    let request_handler =
      Cohttp_lwt_unix.Server.make ~callback:(handler push_signals) ()
    in
    let mode = `TCP (`Port port) in
    let* () = Lwt_io.eprintf "starting server on http://localhost:%d\n" port in
    let ipv4_server = Cohttp_lwt_unix.Server.(create ~mode request_handler) in
    let ipv6_server =
      (* TODO: Ideally we could bind both IPv6 and IPv4 in a dual stack server.
         However, Cohttp depends on Conduit for this, which is not currently able
         to support a dual stack. See https://github.com/mirage/ocaml-conduit/issues/323.

         We need an IPv6 server, because Cotthp_eio will try to bind this first,
         if it is available on the local loop, and if we have an open port at
         [::1] without a sever to handle requests, we'll end up with the
         connection refused.

         For the time being, we fix this by running one server for each case. *)
      let* ipv6_ctx =
        let+ ctx = Conduit_lwt_unix.init ~src:"::1" () in
        Cohttp_lwt_unix.Net.init ~ctx ()
      in
      Cohttp_lwt_unix.Server.(create ~ctx:ipv6_ctx ~mode request_handler)
    in
    Lwt.join [ ipv4_server; ipv6_server ]
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

let default_port =
  String.split_on_char ':' Client.Config.default_url |> function
  (* Extracting the port from 'http://foo:<port>' *)
  | [ _; _; port ] -> int_of_string port
  | _ -> failwith "unexpected format in Client.Config.default_url"

let gather_signals ?(port = default_port) program_to_test =
  Lwt_main.run
  @@
  let stream, push = Lwt_stream.create () in
  let* () =
    Lwt.pick [ Server.run port push; Tested_program.run program_to_test ]
  in
  (* Close out the stream *)
  push None;
  Lwt_stream.to_list stream

(* Just run the server, and print the signals gathered. *)
let run ?(port = default_port) () =
  Lwt_main.run
  @@
  let stream, push = Lwt_stream.create () in
  Lwt.join
    [
      Server.run port push;
      Lwt_stream.iter_s
        (fun s -> Format.asprintf "%a" Signal.Pp.pp s |> Lwt_io.printl)
        stream;
    ]
