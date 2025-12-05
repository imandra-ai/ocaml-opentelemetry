(** Constructing and managing the configuration needed in common by all clients
*)

type t = private {
  debug: bool;
  url_traces: string;  (** Url to send traces *)
  url_metrics: string;  (** Url to send metrics*)
  url_logs: string;  (** Url to send logs *)
  headers: (string * string) list;
      (** API headers sent to the endpoint. Default is none or
          "OTEL_EXPORTER_OTLP_HEADERS" if set. *)
  batch_traces: int option;
      (** Batch traces? If [Some i], then this produces batches of (at most) [i]
          items. If [None], there is no batching.

          Note that traces and metrics are batched separately. Default
          [Some 400]. *)
  batch_metrics: int option;
      (** Batch metrics? If [Some i], then this produces batches of (at most)
          [i] items. If [None], there is no batching.

          Note that traces and metrics are batched separately. Default
          [Some 20]. *)
  batch_logs: int option;
      (** Batch logs? See {!batch_metrics} for details. Default [Some 400] *)
  batch_timeout_ms: int;
      (** Number of milliseconds after which we will emit a batch, even
          incomplete. Note that the batch might take longer than that, because
          this is only checked when a new event occurs or when a tick is
          emitted. Default 2_000. *)
  self_trace: bool;
      (** If true, the OTEL library will also emit its own spans. Default
          [false].
          @since 0.7 *)
}
(** Configuration.

    To build one, use {!make} below. This might be extended with more fields in
    the future. *)

val default_url : string
(** The default base URL for the config. *)

val pp : Format.formatter -> t -> unit

type 'k make =
  ?debug:bool ->
  ?url:string ->
  ?url_traces:string ->
  ?url_metrics:string ->
  ?url_logs:string ->
  ?batch_traces:int option ->
  ?batch_metrics:int option ->
  ?batch_logs:int option ->
  ?headers:(string * string) list ->
  ?batch_timeout_ms:int ->
  ?self_trace:bool ->
  'k
(** A function that gathers all the values needed to construct a {!t}, and
    produces a ['k]. ['k] is typically a continuation used to construct a
    configuration that includes a {!t}.

    @param url
      base url used to construct per-signal urls. Per-signal url options take
      precedence over this base url. If not provided, this defaults to
      "OTEL_EXPORTER_OTLP_ENDPOINT" if set, or if not {!default_url}.

    Example of constructed per-signal urls with the base url
    http://localhost:4318
    - Traces: http://localhost:4318/v1/traces
    - Metrics: http://localhost:4318/v1/metrics
    - Logs: http://localhost:4318/v1/logs

    Use per-signal url options if different urls are needed for each signal
    type.

    @param url_traces
      url to send traces, or "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT" if set. The
      url is used as-is without any modification.

    @param url_metrics
      url to send metrics, or "OTEL_EXPORTER_OTLP_METRICS_ENDPOINT" if set. The
      url is used as-is without any modification.

    @param url_logs
      url to send logs, or "OTEL_EXPORTER_OTLP_LOGS_ENDPOINT" if set. The url is
      used as-is without any modification. *)

(** Construct, inspect, and update {!t} configurations, drawing defaults from
    the environment and encapsulating state *)
module type ENV = sig
  val get_debug : unit -> bool

  val set_debug : bool -> unit

  val get_headers : unit -> (string * string) list

  val set_headers : (string * string) list -> unit

  val make : (t -> 'a) -> 'a make
  (** [make f] is a {!type:make} function that will give [f] a safely
      constructed {!t}.

      Typically this is used to extend the constructor for {!t} with new
      optional arguments.

      E.g., we can construct a configuration that includes a {!t} alongside a
      more specific field like so:

      {[
        type extended_config = {
          new_field: string;
          common: t;
        }

        let make : (new_field:string -> unit -> extended_config) make =
          Env.make (fun common ~new_field () -> { new_field; common })

        let _example : extended_config =
          make ~new_field:"foo" ~url_traces:"foo/bar" ~debug:true ()
      ]}

      As a special case, we can get the simple constructor function for {!t}
      with [Env.make (fun common () -> common)] *)
end

(** A generative functor that produces a state-space that can read configuration
    values from the environment, provide stateful configuration setting and
    accessing operations, and a way to make a new {!t} configuration record *)
module Env : functor () -> ENV
