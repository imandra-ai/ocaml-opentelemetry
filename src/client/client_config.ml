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
  http_concurrency_level: int option;
}

let pp out (self : t) : unit =
  let ppiopt out i =
    match i with
    | None -> Format.fprintf out "None"
    | Some i -> Format.fprintf out "%d" i
  in
  let pp_header ppf (a, b) = Format.fprintf ppf "@[%s: @,%s@]@." a b in
  let ppheaders out l =
    Format.fprintf out "[@[%a@]]" (Format.pp_print_list pp_header) l
  in
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
    http_concurrency_level;
  } =
    self
  in
  Format.fprintf out
    "{@[ debug=%B;@ self_trace=%B; url_traces=%S;@ url_metrics=%S;@ \
     url_logs=%S;@ headers=%a;@ batch_traces=%a;@ batch_metrics=%a;@ \
     batch_logs=%a;@ batch_timeout_ms=%d;@ http_concurrency_level=%a @]}"
    debug self_trace url_traces url_metrics url_logs ppheaders headers ppiopt
    batch_traces ppiopt batch_metrics ppiopt batch_logs batch_timeout_ms ppiopt
    http_concurrency_level

let default_url = "http://localhost:4318"

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
  ?http_concurrency_level:int ->
  'k

module type ENV = sig
  val get_debug : unit -> bool

  val set_debug : bool -> unit

  val get_headers : unit -> (string * string) list

  val set_headers : (string * string) list -> unit

  val make : (t -> 'a) -> 'a make
end

module Env () : ENV = struct
  let debug_ =
    ref
      (match Sys.getenv_opt "OTEL_OCAML_DEBUG" with
      | Some ("1" | "true") -> true
      | _ -> false)

  let get_debug () = !debug_

  let set_debug b = debug_ := b

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
      ?(batch_timeout_ms = 2_000) ?(self_trace = false) ?http_concurrency_level
      =
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
        http_concurrency_level;
      }
end
