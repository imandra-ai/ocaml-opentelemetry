(** Configuration for the ocurl backend *)

type t = {
  bg_threads: int;
      (** Are there background threads, and how many? Default [4]. This will be
          adjusted to be at least [1] and at most [32]. *)
  ticker_thread: bool;
      (** If true, start a thread that regularly checks if signals should be
          sent to the collector. Default [true] *)
  ticker_interval_ms: int;
      (** Interval for ticker thread, in milliseconds. This is only useful if
          [ticker_thread] is [true]. This will be clamped between [2 ms] and
          some longer interval (maximum [60s] currently). Default 500.
          @since 0.7 *)
  common: Opentelemetry_client.Client_config.t;
      (** Common configuration options
          @since 0.12*)
}
(** Configuration.

    To build one, use {!make} below. This might be extended with more fields in
    the future. *)

val pp : Format.formatter -> t -> unit

val make :
  (?bg_threads:int ->
  ?ticker_thread:bool ->
  ?ticker_interval_ms:int ->
  unit ->
  t)
  Opentelemetry_client.Client_config.make
(** Make a configuration {!t}. *)

module Env : Opentelemetry_client.Client_config.ENV
