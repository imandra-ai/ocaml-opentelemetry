(* FIXME: Deadlocks when run with 1 batch *)
let () =
  Trace_collector.run
    ~program_to_test:[ "emit1_cohttp"; "-j"; "1"; "--batch-traces"; "2" ]
    ()
