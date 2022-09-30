open Common_

type t = {
  debug: bool;
  url: string;
  headers: (string * string) list;
  batch_traces: int option;
  batch_metrics: int option;
  batch_logs: int option;
  batch_timeout_ms: int;
}

let pp out self : unit =
  let ppiopt = Format.pp_print_option Format.pp_print_int in
  let pp_header ppf (a, b) = Format.fprintf ppf "@[%s: @,%s@]@." a b in
  let ppheaders = Format.pp_print_list pp_header in
  let {
    debug;
    url;
    headers;
    batch_traces;
    batch_metrics;
    batch_logs;
    batch_timeout_ms;
  } =
    self
  in
  Format.fprintf out
    "{@[ debug=%B;@ url=%S;@ headers=%a;@ batch_traces=%a;@ batch_metrics=%a;@ \
     batch_logs=%a;@ batch_timeout_ms=%d; @]}"
    debug url ppheaders headers ppiopt batch_traces ppiopt batch_metrics ppiopt
    batch_logs batch_timeout_ms

let make ?(debug = !debug_) ?(url = get_url ()) ?(headers = get_headers ())
    ?(batch_traces = Some 400) ?(batch_metrics = Some 20)
    ?(batch_logs = Some 400) ?(batch_timeout_ms = 500) () : t =
  {
    debug;
    url;
    headers;
    batch_traces;
    batch_metrics;
    batch_timeout_ms;
    batch_logs;
  }
