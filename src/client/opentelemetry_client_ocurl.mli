
(*
   TODO: more options from
   https://opentelemetry.io/docs/reference/specification/protocol/exporter/
   *)

val get_url : unit -> string

val set_url : string -> unit
(** Url of the endpoint. Default is "http://localhost:4318",
    or "OTEL_EXPORTER_OTLP_ENDPOINT" if set. *)

val set_mutex : lock:(unit -> unit) -> unlock:(unit -> unit) -> unit

val setup : unit -> unit
(** Setup endpoint. This modifies {!Opentelemetry.Collector.backend}. *)

val with_setup : (unit -> 'a) -> 'a
