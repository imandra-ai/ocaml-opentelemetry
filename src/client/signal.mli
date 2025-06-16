(** Constructing and managing OTel
    {{:https://opentelemetry.io/docs/concepts/signals/} signals} *)

(** Convert signals to protobuf encoded strings, ready to be sent over the wire

    NOTE: The converters share an underlying stateful encoder, so each domain or
    system thread should have its own [Converter] instance *)
module Converter : functor () -> sig
  val logs : Opentelemetry_proto.Logs.resource_logs list -> string
  (** [logs ls] is a protobuf encoded string of the logs [ls] *)

  val metrics : Opentelemetry_proto.Metrics.resource_metrics list -> string
  (** [metrics ms] is a protobuf encoded string of the metrics [ms] *)

  val traces : Opentelemetry_proto.Trace.resource_spans list -> string
  (** [metrics ts] is a protobuf encoded string of the traces [ts] *)
end
