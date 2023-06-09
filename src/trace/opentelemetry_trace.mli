val collector : unit -> Trace.collector
(** Make a Trace collector that uses the OTEL backend to send spans and logs *)

val setup : unit -> unit
(** Install the OTEL backend as a Trace collector *)

val setup_with_otel_backend : Opentelemetry.Collector.backend -> unit
(** Same as {!setup},  but also install the given backend as OTEL backend *)
