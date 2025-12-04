module Otel = Opentelemetry
module Otrace = Trace_core (* ocaml-trace *)
module TSub = Trace_subscriber.Subscriber

let ( let@ ) = ( @@ )

let spf = Printf.sprintf
