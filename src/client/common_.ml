module Atomic = Opentelemetry_atomic.Atomic

let[@inline] (let@) f x = f x

let debug_ = ref (match Sys.getenv_opt "OTEL_OCAML_DEBUG" with Some ("1"|"true") -> true | _ -> false)

let lock_ : (unit -> unit) ref = ref ignore
let unlock_ : (unit -> unit) ref = ref ignore

let set_mutex ~lock ~unlock : unit =
  lock_ := lock;
  unlock_ := unlock

(* critical section for [f()] *)
let[@inline] with_lock_ f =
  !lock_();
  Fun.protect ~finally:!unlock_ f

let[@inline] with_mutex_ m f =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) f

let parse_headers s =
  let parse_header s = Scanf.sscanf s "%s@=%s" (fun key value -> key, value) in
  String.split_on_char ',' s |> List.map parse_header

let default_url = "http://localhost:4318"
let default_headers = []
let url = ref (try Sys.getenv "OTEL_EXPORTER_OTLP_ENDPOINT" with _ -> default_url)
let headers = ref (try parse_headers (Sys.getenv "OTEL_EXPORTER_OTLP_HEADERS") with _ -> default_headers)
let get_url () = !url
let set_url s = url := s

let get_headers () = !headers
let set_headers s = headers := s
