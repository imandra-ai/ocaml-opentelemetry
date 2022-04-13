
(*
   TODO: more options from
   https://opentelemetry.io/docs/reference/specification/protocol/exporter/
   *)

val get_url : unit -> string

val set_url : string -> unit
(** Url of the endpoint. Default is "http://localhost:4318",
    or "OTEL_EXPORTER_OTLP_ENDPOINT" if set. *)

val set_mutex : lock:(unit -> unit) -> unlock:(unit -> unit) -> unit
(** Set a lock/unlock pair to protect the critical sections
    of {!Opentelemetry.Collector.BACKEND} *)

module Config = Config

val setup : ?config:Config.t -> ?enable:bool -> unit -> unit
(** Setup endpoint. This modifies {!Opentelemetry.Collector.backend}.
    @param enable actually setup the backend (default true). This can
      be used to enable/disable the setup depending on CLI arguments
      or environment.
    @param config configuration to use *)

val with_setup : ?config:Config.t -> ?enable:bool -> unit -> (unit -> 'a) -> 'a
(** [with_setup () f] is like [setup(); f()] but takes care of cleaning up
    after [f()] returns. *)
