(** Constructing and managing OTel
    {{:https://opentelemetry.io/docs/concepts/signals/} signals} *)

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
  (** [metrics ts] is a protobuf encoded string of the traces [ts]

      @param encoder provide an encoder state to reuse *)
end
