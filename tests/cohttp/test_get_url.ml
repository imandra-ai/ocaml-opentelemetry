let url = "http://localhost:3000"

let cohttp () =
  let config = Opentelemetry_client_cohttp_lwt.Config.make ~url () in
  Opentelemetry_client_cohttp_lwt.with_setup ~config () @@ fun () ->
  print_endline @@ Printf.sprintf "cohttp url = %s" url

let () = cohttp ()
