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
      only checked when a new event occurs. Default 500. *)
  bg_threads: int;
      (** Are there background threads, and how many? Default [4] *)
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
  unit ->
  t
(** Make a configuration.
 *)

val pp : Format.formatter -> t -> unit
