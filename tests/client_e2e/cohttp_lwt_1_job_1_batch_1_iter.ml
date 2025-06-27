(* FIXME: Deadlocks when run with 1 batch *)
let () =
  Signal_collector.run
    ~program_to_test:[ "emit1_cohttp"; "-j"; "1"; "--batch-traces"; "1" ]
    ()
