let () =
  Signal_collector.run
    ~program_to_test:
      [
        "emit1_cohttp";
        "-j";
        "1";
        "--batch-traces";
        "2";
        "--batch-metrics";
        "2";
        "--batch-logs";
        "2";
      ]
    ()
