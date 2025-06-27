let () =
  let program_to_test = Sys.argv |> Array.to_list |> List.tl in
  Trace_collector.run ~program_to_test ()
