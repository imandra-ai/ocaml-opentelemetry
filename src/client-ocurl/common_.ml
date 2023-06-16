module Atomic = Opentelemetry_atomic.Atomic
include Opentelemetry.Lock

let spf = Printf.sprintf

let ( let@ ) = ( @@ )

let tid () = Thread.id @@ Thread.self ()

let debug_ =
  ref
    (match Sys.getenv_opt "OTEL_OCAML_DEBUG" with
    | Some ("1" | "true") -> true
    | _ -> false)

let default_url = "http://localhost:4318"

let url =
  ref (try Sys.getenv "OTEL_EXPORTER_OTLP_ENDPOINT" with _ -> default_url)

let get_url () = !url

let set_url s = url := s

let parse_headers s =
  let parse_header s =
    match String.split_on_char '=' s with
    | [ key; value ] -> key, value
    | _ -> failwith "Unexpected format for header"
  in
  String.split_on_char ',' s |> List.map parse_header

let default_headers = []

let headers =
  ref
    (try parse_headers (Sys.getenv "OTEL_EXPORTER_OTLP_HEADERS")
     with _ -> default_headers)

let get_headers () = !headers

let set_headers s = headers := s
