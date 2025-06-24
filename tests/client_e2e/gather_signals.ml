let () =
  let program_to_test = Sys.argv |> Array.to_list |> List.tl in
  Signal_gatherer.run ~program_to_test ()
