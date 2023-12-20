(** Configuration for the ocurl backend *)

type t = private {
  debug: bool;
  url: string;
      (** Url of the endpoint. Default is "http://localhost:4318",
      or "OTEL_EXPORTER_OTLP_ENDPOINT" if set. *)
  headers: (string * string) list;
      (** API headers sent to the endpoint. Default is none or
      "OTEL_EXPORTER_OTLP_HEADERS" if set. *)
  batch_timeout_ms: int;
      (** Number of milliseconds after which we will emit a batch, even
      incomplete.
      Note that the batch might take longer than that, because this is
      only checked when a new event occurs or when a tick
      is emitted. Default 2_000. *)
  bg_threads: int;
      (** Are there background threads, and how many? Default [4] *)
  ticker_thread: bool;
      (** If true, start a thread that regularly checks if signals should
          be sent to the collector. Default [true] *)
  ticker_interval_ms: int;
      (** Interval for ticker thread, in milliseconds. This is
          only useful if [ticker_thread] is [true].
      Default 500.
      @since NEXT_RELEASE *)
  self_trace: bool;
      (** If true, the OTEL library will also emit its own spans.
          @since NEXT_RELEASE *)
}
(** Configuration.

  To build one, use {!make} below. This might be extended with more
  fields in the future. *)

val make :
  ?debug:bool ->
  ?url:string ->
  ?headers:(string * string) list ->
  ?batch_timeout_ms:int ->
  ?bg_threads:int ->
  ?ticker_thread:bool ->
  ?ticker_interval_ms:int ->
  ?self_trace:bool ->
  unit ->
  t
(** Make a configuration.
 *)

val pp : Format.formatter -> t -> unit
