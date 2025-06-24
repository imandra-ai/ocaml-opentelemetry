let test = Test_server_lib.Tests.test

module Client = Opentelemetry_client
open Client.Signal

(* Max batch size for the different signals  *)
let batch_traces = 2

let ( let* ) = Option.bind

let ( let+ ) x f = Option.map f x

open Alcotest

(* TODO: Running with batch-traces = 1 causes deadlocks *)
let () =
  Test_server_lib.run_tests
    ~program_to_test:
      [|
        "emit1_cohttp"; "-j"; "2"; "--batch-traces"; string_of_int batch_traces;
      |]
    ~suite_name:"lwt-cohttp client"
    [
      test ~name:"WIP" (fun ts ->
          let+ ts = to_traces ts in
          fun () ->
            check' int ~msg:"batch is of configured size" ~actual:batch_traces
              ~expected:(List.length ts);
            let id =
              (((List.nth ts 0).scope_spans |> Fun.flip List.nth 0).spans
             |> Fun.flip List.nth 0)
                .trace_id
            in
            Format.eprintf ">>> %a@." Pp.traces ts;
            Format.eprintf ">>> TRACE ID: %a@." Pbrt.Pp.pp_bytes id;
            Format.eprintf ">>> FOOO";
            Alcotest.fail "Dbg");
      (* test ~name:"Fails" *)
      (*   ~condition:(function *)
      (*     | _ -> true) *)
      (*   (fun _ -> Alcotest.fail "Failing"); *)
    ]
