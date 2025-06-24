(* A runs tests against a OTel-instrumented program  *)

module Client = Opentelemetry_client
module Signal = Client.Signal
module Proto = Opentelemetry.Proto

let () = Logs.set_reporter (Logs_fmt.reporter ())

and () = Logs.Src.set_level Cohttp_eio.src (Some Warning)

(* Record and output events observed by the server *)
module Record = struct
  let traceln_request kind req pp data =
    Eio.traceln "# received %s\nREQUEST: %a\nBODY: %a\n@." kind Http.Request.pp
      req pp data

  let traces requests req data =
    let traces = data |> Eio.Flow.read_all |> Client.Signal.Decode.traces in
    Eio.Stream.add requests (Client.Signal.Traces traces);
    traceln_request "trace" req Signal.Pp.traces traces

  let metrics requests req data =
    let metrics = data |> Eio.Flow.read_all |> Client.Signal.Decode.metrics in
    Eio.Stream.add requests (Client.Signal.Metrics metrics);
    traceln_request "metrics" req Signal.Pp.metrics metrics

  let logs requests req data =
    let logs = data |> Eio.Flow.read_all |> Client.Signal.Decode.logs in
    Eio.Stream.add requests (Client.Signal.Logs logs);
    traceln_request "logs" req Signal.Pp.logs logs
end

(* Server to collect telemetry data *)
module Server = struct
  let handler reqs _socket request body =
    let resp_body = Cohttp_eio.Body.of_string "" in
    match Http.Request.resource request with
    | "/v1/traces" ->
      Record.traces reqs request body;
      Cohttp_eio.Server.respond ~status:`OK ~body:resp_body ()
    | "/v1/metrics" ->
      Record.metrics reqs request body;
      Cohttp_eio.Server.respond ~status:`OK ~body:resp_body ()
    | "/v1/logs" ->
      Record.logs reqs request body;
      Cohttp_eio.Server.respond ~status:`OK ~body:resp_body ()
    | unexepected ->
      Eio.traceln "unexpected endpoint %s" unexepected;
      Eio.traceln "REQUEST: %a\n@." Http.Request.pp request;
      Eio.traceln "BODY: %s\n@." (Eio.Flow.read_all body);
      Cohttp_eio.Server.respond ~status:`Not_found ~body:resp_body ()

  let log_warning ex = Logs.warn (fun f -> f "%a" Eio.Exn.pp ex)

  let run ~env ~sw ~port requests () =
    let socket =
      Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    and server = Cohttp_eio.Server.make ~callback:(handler requests) () in
    Cohttp_eio.Server.run socket server ~on_error:log_warning
end

(** Manage launching and cleaning up the program we are testing *)
module Tested_program = struct
  let cleanup proc =
   fun () ->
    match Unix.close_process proc with
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

  let run ~sw cmd () =
    let program = cmd.(0) in
    let proc = Unix.open_process_args program cmd in
    let pid = Unix.process_pid proc in
    at_exit (fun () -> Unix.kill pid 9);
    Eio.Switch.on_release sw (cleanup proc)
end

module Tests = struct
  type t = {
    name: string;
    condition: Signal.t -> bool;
    test: Signal.t -> unit;
        (** A test that is runnable iff the given signal satisfies the condition
        *)
  }

  let test ~name ~condition test = { name; test; condition }

  let take_first f l =
    let rec search keep = function
      | [] -> None
      | x :: xs ->
        if f x then
          Some (x, List.rev_append keep xs)
        else
          search (x :: keep) xs
    in
    search [] l

  let derive_alcotest tests signals : unit Alcotest.test_case list =
    let rec find_tests conditional_tests runnable_tests =
      let signal = Eio.Stream.take signals in
      match conditional_tests with
      | [] ->
        (* We collected all the tests *)
        runnable_tests
      | rest ->
        (match take_first (fun t -> t.condition signal) rest with
        | None -> find_tests rest runnable_tests
        | Some (t, rest') ->
          Eio.traceln "found test %s" t.name;
          let test =
            Alcotest.test_case t.name `Quick (fun () -> t.test signal)
          in
          find_tests rest' (test :: runnable_tests))
    in
    find_tests tests [] |> List.rev

  let run suite_name tests clock signals () =
    (* Ensure server is up *)
    Eio.Time.sleep clock 0.5;

    let tests = derive_alcotest tests signals in

    (* If we are not able to derive and run the tests within this limit, it
       probably means the test program is not producing all the signals needed by
       our tests  *)
    let timeout_limit = 10. in
    Eio.traceln "running test without timeout ";
    Eio.Time.with_timeout_exn clock timeout_limit (fun () ->
        Alcotest.run "client_e2e tests" [ suite_name, tests ])
end

let default_port =
  String.split_on_char ':' Client.Config.default_url |> function
  (* Extracting the port from 'http://foo:<port>' *)
  | [ _; _; port ] -> int_of_string port
  | _ -> failwith "unexpected format in Client.Config.default_url"

let run_tests ?(port = default_port) ~cmd (suite, tests) =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let signals = Eio.Stream.create max_int in
  Eio.Fiber.first
    (fun () ->
      Eio.Fiber.both
        (Server.run ~env ~sw ~port signals)
        (Tested_program.run ~sw cmd))
    (Tests.run suite tests env#clock signals)
