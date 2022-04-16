
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

  batch_logs : int option;
  (** Batch logs? See {!batch_metrics} for details.
      Default [Some 400] *)

  batch_timeout_ms: int;
  (** Number of milliseconds after which we will emit a batch, even
      incomplete.
      Note that the batch might take longer than that, because this is
      only checked when a new event occurs. Default 500. *)

  thread: bool;
  (** Is there a background thread? Default [true] *)

  ticker_thread: bool;
  (** Is there a ticker thread? Default [true].
      This thread will regularly call [tick()] on the backend, to make
      sure it makes progress, and regularly send events to the collector.
      This option is ignored if [thread=false]. *)
}

val make :
  ?debug:bool -> ?url:string ->
  ?batch_traces:int option ->
  ?batch_metrics:int option ->
  ?batch_logs:int option ->
  ?batch_timeout_ms:int ->
  ?thread:bool ->
  ?ticker_thread:bool ->
  unit -> t
(** Make a configuration *)

val pp : Format.formatter -> t -> unit
