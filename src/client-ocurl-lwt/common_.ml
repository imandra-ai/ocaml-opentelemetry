module Atomic = Opentelemetry_atomic.Atomic

let[@inline] ( let@ ) f x = f x

let spf = Printf.sprintf

let tid () = Thread.id @@ Thread.self ()
