module Client = Opentelemetry_client
module Proto = Opentelemetry.Proto
open Clients_e2e_lib

let () =
  Clients_e2e_lib.run_tests
    [
      (* TODO: Running with batch-traces = 1 causes deadlocks *)
      (* ( "emit1_cohttp", *)
      (*   { *)
      (*     jobs = 1; *)
      (*     iterations = 1; *)
      (*     batch_traces = 1; *)
      (*     batch_metrics = 1; *)
      (*     batch_logs = 1; *)
      (*   } ); *)
      ( "emit1_cohttp",
        {
          jobs = 1;
          iterations = 1;
          batch_traces = 2;
          batch_metrics = 2;
          batch_logs = 2;
        } );
      ( "emit1_cohttp",
        {
          jobs = 3;
          iterations = 1;
          batch_traces = 400;
          batch_metrics = 3;
          batch_logs = 400;
        } );
    ]
