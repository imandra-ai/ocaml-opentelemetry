(** Utilities for writing clients

    These are used for implementing e.g., the [opentelemetry-client-cohttp-lwt]
    and [opentelemetry-client-ocurl] packages package. *)

(** Constructing and managing the configuration needed in common by all clients
*)
module Config : sig
  type t = private {
    debug: bool;
    url_traces: string;  (** Url to send traces *)
    url_metrics: string;  (** Url to send metrics*)
    url_logs: string;  (** Url to send logs *)
    headers: (string * string) list;
        (** API headers sent to the endpoint. Default is none or
            "OTEL_EXPORTER_OTLP_HEADERS" if set. *)
    batch_traces: int option;
        (** Batch traces? If [Some i], then this produces batches of (at most)
            [i] items. If [None], there is no batching.

            Note that traces and metrics are batched separately. Default
            [Some 400]. *)
    batch_metrics: int option;
        (** Batch metrics? If [Some i], then this produces batches of (at most)
            [i] items. If [None], there is no batching.

            Note that traces and metrics are batched separately. Default [None].
        *)
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

      To build one, use {!make} below. This might be extended with more fields
      in the future. *)

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
        precedence over this base url. Default is "http://localhost:4318", or
        "OTEL_EXPORTER_OTLP_ENDPOINT" if set.

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
        url to send metrics, or "OTEL_EXPORTER_OTLP_METRICS_ENDPOINT" if set.
        The url is used as-is without any modification.

      @param url_logs
        url to send logs, or "OTEL_EXPORTER_OTLP_LOGS_ENDPOINT" if set. The url
        is used as-is without any modification. *)

  (** Construct, inspect, and update {!t} configurations, drawing defaults from
      the environment and encapsulating state *)
  module type Env = sig
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
          type extended_confg =
            { new_field: string
            ; common: t
            }

          let make : (?new_field -> unit) make =
            Env.make (fun common ?new_field () -> {new_field; common})

          let _example : extended_config =
            make ~new_field:"foo" ~url_traces:"foo/bar" ~debug:true ()
        ]}

        As a special case, we can get the simple constructor function for {!t}
        with [Env.make (fun common () -> common)] *)
  end

  (** A generative functor that produces a state-space that can read
      configuration values from the environment, provide stateful configuration
      setting and accessing operations, and a way to make a new {!t}
      configuration record *)
  module Env : functor () -> Env
end = struct
  type t = {
    debug: bool;
    url_traces: string;
    url_metrics: string;
    url_logs: string;
    headers: (string * string) list;
    batch_traces: int option;
    batch_metrics: int option;
    batch_logs: int option;
    batch_timeout_ms: int;
    self_trace: bool;
  }

  let pp out (self : t) : unit =
    let ppiopt = Format.pp_print_option Format.pp_print_int in
    let pp_header ppf (a, b) = Format.fprintf ppf "@[%s: @,%s@]@." a b in
    let ppheaders = Format.pp_print_list pp_header in
    let {
      debug;
      self_trace;
      url_traces;
      url_metrics;
      url_logs;
      headers;
      batch_traces;
      batch_metrics;
      batch_logs;
      batch_timeout_ms;
    } =
      self
    in
    Format.fprintf out
      "{@[ debug=%B;@ self_trace=%B; url_traces=%S;@ url_metrics=%S;@ \
       url_logs=%S;@ headers=%a;@ batch_traces=%a;@ batch_metrics=%a;@ \
       batch_logs=%a;@ batch_timeout_ms=%d @]}"
      debug self_trace url_traces url_metrics url_logs ppheaders headers ppiopt
      batch_traces ppiopt batch_metrics ppiopt batch_logs batch_timeout_ms

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

  module type Env = sig
    val get_debug : unit -> bool

    val set_debug : bool -> unit

    val get_headers : unit -> (string * string) list

    val set_headers : (string * string) list -> unit

    val make : (t -> 'a) -> 'a make
  end

  module Env () : Env = struct
    let debug_ =
      ref
        (match Sys.getenv_opt "OTEL_OCAML_DEBUG" with
        | Some ("1" | "true") -> true
        | _ -> false)

    let get_debug () = !debug_

    let set_debug b = debug_ := b

    let default_url = "http://localhost:4318"

    let make_get_from_env env_name =
      let value = ref None in
      fun () ->
        match !value with
        | None ->
          value := Sys.getenv_opt env_name;
          !value
        | Some value -> Some value

    let get_url_from_env = make_get_from_env "OTEL_EXPORTER_OTLP_ENDPOINT"

    let get_url_traces_from_env =
      make_get_from_env "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"

    let get_url_metrics_from_env =
      make_get_from_env "OTEL_EXPORTER_OTLP_METRICS_ENDPOINT"

    let get_url_logs_from_env =
      make_get_from_env "OTEL_EXPORTER_OTLP_LOGS_ENDPOINT"

    let remove_trailing_slash url =
      if url <> "" && String.get url (String.length url - 1) = '/' then
        String.sub url 0 (String.length url - 1)
      else
        url

    let parse_headers s =
      let parse_header s =
        match String.split_on_char '=' s with
        | [ key; value ] -> key, value
        | _ -> failwith "Unexpected format for header"
      in
      String.split_on_char ',' s |> List.map parse_header

    let default_headers = []

    let headers =
      ref
        (try parse_headers (Sys.getenv "OTEL_EXPORTER_OTLP_HEADERS")
         with _ -> default_headers)

    let get_headers () = !headers

    let set_headers s = headers := s

    let make k ?(debug = get_debug ()) ?url ?url_traces ?url_metrics ?url_logs
        ?(batch_traces = Some 400) ?(batch_metrics = Some 20)
        ?(batch_logs = Some 400) ?(headers = get_headers ())
        ?(batch_timeout_ms = 2_000) ?(self_trace = false) =
      (* Ensure the state is synced, in case these values are passed in explicitly *)
      set_debug debug;
      set_headers headers;
      let url_traces, url_metrics, url_logs =
        let base_url =
          let base_url =
            match get_url_from_env () with
            | None -> Option.value url ~default:default_url
            | Some url -> remove_trailing_slash url
          in
          remove_trailing_slash base_url
        in
        let url_traces =
          match get_url_traces_from_env () with
          | None -> Option.value url_traces ~default:(base_url ^ "/v1/traces")
          | Some url -> url
        in
        let url_metrics =
          match get_url_metrics_from_env () with
          | None -> Option.value url_metrics ~default:(base_url ^ "/v1/metrics")
          | Some url -> url
        in
        let url_logs =
          match get_url_logs_from_env () with
          | None -> Option.value url_logs ~default:(base_url ^ "/v1/logs")
          | Some url -> url
        in
        url_traces, url_metrics, url_logs
      in
      k
        {
          debug;
          url_traces;
          url_metrics;
          url_logs;
          headers;
          batch_traces;
          batch_metrics;
          batch_logs;
          batch_timeout_ms;
          self_trace;
        }
  end
end
