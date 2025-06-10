module Atomic = Opentelemetry_atomic.Atomic
include Opentelemetry.Lock

let spf = Printf.sprintf

let ( let@ ) = ( @@ )

let tid () = Thread.id @@ Thread.self ()
