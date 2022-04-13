module Atomic = Opentelemetry_atomic.Atomic

let[@inline] (let@) f x = f x

let debug_ = ref (try bool_of_string @@ Sys.getenv "DEBUG" with _ -> false)

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

let default_url = "http://localhost:4318"
let url = ref (try Sys.getenv "OTEL_EXPORTER_OTLP_ENDPOINT" with _ -> default_url)
let get_url () = !url
let set_url s = url := s
