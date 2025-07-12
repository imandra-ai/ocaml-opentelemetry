(** Runs a signal gatherer server, and prints out every batch of signals
    received to stdout. This can be used to monitor the signals sent by an
    application, e.g., the test executables defined in /tests/bin/emit1*.ml *)
let () = Signal_gatherer.run ()
