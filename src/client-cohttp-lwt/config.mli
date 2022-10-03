type t = private {
  debug: bool;
  url: string;
      (** Url of the endpoint. Default is "http://localhost:4318",
      or "OTEL_EXPORTER_OTLP_ENDPOINT" if set. *)
  headers: (string * string) list;
      (** API headers sent to the endpoint. Default is none or
      "OTEL_EXPORTER_OTLP_HEADERS" if set. *)
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
  batch_logs: int option;
      (** Batch logs? See {!batch_metrics} for details.
      Default [Some 400] *)
  batch_timeout_ms: int;
      (** Number of milliseconds after which we will emit a batch, even
      incomplete.
      Note that the batch might take longer than that, because this is
      only checked when a new event occurs. Default 500. *)
}
(** Configuration.

  To build one, use {!make} below. This might be extended with more
  fields in the future. *)

val make :
  ?debug:bool ->
  ?url:string ->
  ?headers:(string * string) list ->
  ?batch_traces:int option ->
  ?batch_metrics:int option ->
  ?batch_logs:int option ->
  ?batch_timeout_ms:int ->
  unit ->
  t
(** Make a configuration.

   @param thread if true and [bg_threads] is not provided, we will pick a number
   of bg threads. Otherwise the number of [bg_threads] superseeds this option.

 *)

val pp : Format.formatter -> t -> unit
