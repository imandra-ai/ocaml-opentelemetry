module Atomic = Opentelemetry_atomic.Atomic
module Proto = Opentelemetry_proto

let spf = Printf.sprintf

let ( let@ ) = ( @@ )

let[@inline] tid () = Thread.id @@ Thread.self ()
