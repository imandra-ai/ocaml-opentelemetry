
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

module Config : sig
  type t = {
    debug: bool;

    url: string;
    (** Url of the endpoint. Default is "http://localhost:4318",
        or "OTEL_EXPORTER_OTLP_ENDPOINT" if set. *)

    batch_traces: int option;
    (** Batch traces? If [Some i], then this produces batches of (at most)
        [i] items. If [None], there is no batching.

        Note that traces and metrics are batched separately.
        Default [Some 400].
    *)

    batch_metrics: int option;
    (** Batch metrics? If [Some i], then this produces batches of (at most)
        [i] items. If [None], there is no batching.

        Note that traces and metrics are batched separately.
        Default [None].
    *)

    thread: bool;
    (** Is there a background thread? Default [true] *)
  }

  val make :
    ?debug:bool -> ?url:string ->
    ?batch_traces:int option ->
    ?batch_metrics:int option ->
    ?thread:bool ->
    unit -> t
  (** Make a configuration *)

  val pp : Format.formatter -> t -> unit
end

val setup : ?config:Config.t -> unit -> unit
(** Setup endpoint. This modifies {!Opentelemetry.Collector.backend}.
    @param config configuration to use *)

val with_setup : ?config:Config.t -> (unit -> 'a) -> 'a
