let url = "http://localhost:3000"

let ocurl () =
  let config = Opentelemetry_client_ocurl.Config.make ~url () in
  Opentelemetry_client_ocurl.with_setup ~config () @@ fun () ->
  let url = Opentelemetry_client_ocurl.get_url () in
  print_endline @@ Printf.sprintf "ocurl url = %s" url

let cohttp () =
  let config = Opentelemetry_client_cohttp_lwt.Config.make ~url () in
  Opentelemetry_client_cohttp_lwt.with_setup ~config () @@ fun () ->
  let url = Opentelemetry_client_cohttp_lwt.get_url () in
  print_endline @@ Printf.sprintf "cohttp url = %s" url

let () =
  ocurl ();
  cohttp ()
