(* A runs tests against a OTel-instrumented program  *)

module Client = Opentelemetry_client
module Signal = Client.Signal
module Proto = Opentelemetry.Proto

let () = Logs.set_reporter (Logs_fmt.reporter ())

and () = Logs.Src.set_level Cohttp_eio.src (Some Warning)

module Tests = struct
  type t = {
    name: string;
    test: Signal.t -> (unit -> unit) option;
        (** [test s] should be [Some test] iff [test] is meant to be run on the
            signal [s] *)
  }

  let test ~name test = { name; test }

  let derive_alcotest tests signals : unit Alcotest.test_case list =
    let rec find_tests conditional_tests runnable_tests =
      let signal = Eio.Stream.take signals in
      match conditional_tests with
      | [] ->
        (* We collected all the tests *)
        runnable_tests
      | rest ->
        let test_cases, conditionals =
          rest
          |> List.partition_map (fun t ->
                 match t.test signal with
                 | Some test -> Left (Alcotest.test_case t.name `Quick test)
                 | None -> Right t)
        in
        find_tests conditionals (test_cases @ runnable_tests)
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
    Eio.traceln "running test with timeout ";
    Eio.Time.with_timeout_exn clock timeout_limit (fun () ->
        Alcotest.run "client_e2e tests" [ suite_name, tests ])
end

(* Record and output events observed by the server *)
module Record = struct
  let traceln_request kind req pp data =
    let _ = kind, req, pp, data in
    (****** NOTE: Uncomment for debugging *)
    (* let () = *)
    (*   Eio.traceln "# received %s\nREQUEST: %a\nBODY: %a\n@." kind *)
    (*     Http.Request.pp req pp data *)
    (* in *)
    ()

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

  let run socket requests () =
    Eio.traceln "starting server";
    try
      let server = Cohttp_eio.Server.make ~callback:(handler requests) () in
      let () = Cohttp_eio.Server.run socket server ~on_error:log_warning in
      `Stop_daemon
    with Sys_error err ->
      Eio.traceln "Caught sys error %s" err;
      `Stop_daemon
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

  let cleanup proc =
   fun () ->
    Eio.traceln "waiting for proc to end";
    Unix.close_process proc |> validate_exit

  let run ~sw cmd () =
    let program = cmd.(0) in
    Eio.traceln "opening proc";
    let proc = Unix.open_process_args program cmd in
    let pid = Unix.process_pid proc in
    at_exit (fun () -> Unix.kill pid 9);
    Eio.Switch.on_release sw (cleanup proc);
    let rec loop () =
      match In_channel.input_line (fst proc) with
      | Some l ->
        Eio.traceln "pid (stdout): %d %s" pid l;
        loop ()
      | None -> ()
    in
    Eio.traceln "pid (stdout): %d done reading" pid;
    loop ();
    Unix.waitpid [] pid |> snd |> validate_exit
end

let default_port =
  String.split_on_char ':' Client.Config.default_url |> function
  (* Extracting the port from 'http://foo:<port>' *)
  | [ _; _; port ] -> int_of_string port
  | _ -> failwith "unexpected format in Client.Config.default_url"

let collect_traces ~env ~sw ~port program_to_test signals () =
  (* Create socket first, ensuring it will be available for tested program's collector *)
  let socket =
    Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Eio.Fiber.fork_daemon ~sw (Server.run socket signals);
  let proc_mgr = Eio.Stdenv.process_mgr env in
  Eio.Process.run ~stdout:(Eio.Stdenv.stderr env) proc_mgr program_to_test

let take_all (s : 'a Eio.Stream.t) : 'a list =
  let rec loop ls =
    match Eio.Stream.take_nonblocking s with
    | None -> List.rev ls
    | Some x -> loop (x :: ls)
  in
  loop []

(* let compare_signals a b = *)
(*   match a, b with *)
(*   | Signal.Logs _, Signal.Logs _ *)
(*   | Signal.Metrics _, Signal.Metrics _ *)
(*   | Signal.Traces _, Signal.Traces _ -> 0 *)
(*   | Signal.Logs _, _ -> -1 *)
(*   | Signal.Metrics,  *)

(* type event = { *)
(*   name: string; *)
(*   attributes: (string * string) list; *)
(* } *)

(* type data_point = { *)
(*   value: string; *)
(*   attributes: (string * string) list; *)
(* } *)

(* type span_data = *)
(*   { *)
(*   name: string; *)
(*   attributes: (string * string) list; *)
(*   events: event list *)
(*   } *)

(* type scope_data =  { *)
(*   scope: string; *)
(*   spans: span_data list *)
(* } *)

(* type trace_data = *)
(*   { *)
(*     resource: string; *)
(*     spans : span_data list *)
(*   } *)

(* type signal_data = *)
(*   | Trace of trace_data *)
(*   | Log of { *)
(*       body: string; *)
(*       severity: string; *)
(*       attributes: (string * string) list; *)
(*     } *)
(*   | Metrics of { *)
(*       scope: string; *)
(*       name: string; *)
(*       description: string; *)
(*       unit: string; *)
(*       data_points: data_point list; *)
(*       is_monotonic: bool; *)
(*     } *)

(* let trace_to_trace_data : Proto.Trace.resource_spans -> trace_data = fun spans -> *)
(*   let resource = spans.resource |> Option.fold ~some:(Format.asprintf "%a" Proto.Resource.pp_resource) ~none:"" in *)
(*   let span_data : span_data = *)
(*     spans.scope_spans |> List.map (fun s -> *)
(*         { name : s.scope |> Option.fold ~some:(fun scope -> scope.name) ~none:"" *)
(*         ; a} *)
(*       ) *)

(* let signals_to_signal_data : Signal.t -> signal_data list = function *)
(*   | Traces ts -> *)
(*     let scope = List.nth ts 0 in *)
(*     {} *)

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

let run ?(port = default_port) ~program_to_test () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run ~name:"main" @@ fun _sw ->
  let signals = Eio.Stream.create max_int in
  ( Eio.Switch.run ~name:"collector" @@ fun collector_sw ->
    collect_traces ~env ~sw:collector_sw ~port program_to_test signals () );
  take_all signals
  |> List.map (fun s -> s |> Format.asprintf "%a" Signal.Pp.pp |> normalize)
  |> List.stable_sort String.compare (* Produce a deterministic order *)
  |> List.iter print_string
