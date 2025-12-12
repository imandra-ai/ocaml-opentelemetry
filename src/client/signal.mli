(** Constructing and managing OTel
    {{:https://opentelemetry.io/docs/concepts/signals/} signals} *)

(** The type of signals

    This is not the principle type of signals from the perspective of what gets
    encoded and sent via protocl buffers, but it is the principle type that
    collector clients needs to reason about. *)
type t =
  | Traces of Opentelemetry_proto.Trace.resource_spans list
  | Metrics of Opentelemetry_proto.Metrics.resource_metrics list
  | Logs of Opentelemetry_proto.Logs.resource_logs list

val to_traces : t -> Opentelemetry_proto.Trace.resource_spans list option

val to_metrics : t -> Opentelemetry_proto.Metrics.resource_metrics list option

val to_logs : t -> Opentelemetry_proto.Logs.resource_logs list option

val is_traces : t -> bool

val is_metrics : t -> bool

val is_logs : t -> bool

(** Encode signals to protobuf encoded strings, ready to be sent over the wire
*)
module Encode : sig
  val logs :
    ?encoder:Pbrt.Encoder.t ->
    Opentelemetry_proto.Logs.resource_logs list ->
    string
  (** [logs ls] is a protobuf encoded string of the logs [ls]

      @param encoder provide an encoder state to reuse *)

  val metrics :
    ?encoder:Pbrt.Encoder.t ->
    Opentelemetry_proto.Metrics.resource_metrics list ->
    string
  (** [metrics ms] is a protobuf encoded string of the metrics [ms]
      @param encoder provide an encoder state to reuse *)

  val traces :
    ?encoder:Pbrt.Encoder.t ->
    Opentelemetry_proto.Trace.resource_spans list ->
    string
  (** [traces ts] is a protobuf encoded string of the traces [ts]

      @param encoder provide an encoder state to reuse *)

  val any : ?encoder:Pbrt.Encoder.t -> Any_resource.t -> string
end

(** Decode signals from protobuf encoded strings, received over the wire *)
module Decode : sig
  val logs : string -> Opentelemetry_proto.Logs.resource_logs list
  (** [logs s] is a list of log resources decoded from the protobuf encoded
      string [s].

      @raise Pbrt.Decoder.Failure if [s] is not a valid protobuf encoding. *)

  val metrics : string -> Opentelemetry_proto.Metrics.resource_metrics list
  (** [metrics s] is a list of metrics resources decoded from the protobuf
      encoded string [s].

      @raise Pbrt.Decoder.Failure if [s] is not a valid protobuf encoding. *)

  val traces : string -> Opentelemetry_proto.Trace.resource_spans list
  (** [traces s] is a list of span resources decoded from the protobuf encoded
      string [s].

      @raise Pbrt.Decoder.Failure if [s] is not a valid protobuf encoding. *)
end

module Pp : sig
  val logs :
    Format.formatter -> Opentelemetry_proto.Logs.resource_logs list -> unit

  val metrics :
    Format.formatter ->
    Opentelemetry_proto.Metrics.resource_metrics list ->
    unit

  val traces :
    Format.formatter -> Opentelemetry_proto.Trace.resource_spans list -> unit

  val pp : Format.formatter -> t -> unit
end
