let test = Test_server_lib.Tests.test

module Client = Opentelemetry_client
open Client.Signal

let to_traces = function
  | Traces tr -> tr
  | _ -> failwith "NON"

let tests =
  ( "lwt-cohttp client",
    [
      test ~name:"TODO"
        ~condition:(function
          | _ -> true)
        (fun _ -> ());
    ] )

let () = Test_server_lib.run_tests ~cmd:[| "emit1_cohttp"; "-j"; "2" |] tests
