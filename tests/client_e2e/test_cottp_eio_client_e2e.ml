module Client = Opentelemetry_client
module Proto = Opentelemetry.Proto
open Clients_e2e_lib

(* NOTE: This port must be different from that used by other integration tests,
   to prevent socket binding clashes. *)
let port = 4328

let url = Printf.sprintf "http://localhost:%d" port

let () =
  Clients_e2e_lib.run_tests ~port
    [
      ( "emit1_eio",
        {
          url;
          jobs = 1;
          procs = 1;
          iterations = 1;
          batch_traces = 2;
          batch_metrics = 2;
          batch_logs = 2;
        } );
      ( "emit1_eio",
        {
          url;
          jobs = 3;
          procs = 1;
          iterations = 1;
          batch_traces = 400;
          batch_metrics = 3;
          batch_logs = 400;
        } );
      ( "emit1_eio",
        {
          url;
          jobs = 3;
          procs = 3;
          iterations = 1;
          batch_traces = 400;
          batch_metrics = 3;
          batch_logs = 400;
        } );
    ]
