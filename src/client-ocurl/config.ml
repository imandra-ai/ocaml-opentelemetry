open Common_

type t = {
  debug: bool;
  url: string;
  headers: (string * string) list;
  batch_traces: int option;
  batch_metrics: int option;
  batch_logs: int option;
  batch_timeout_ms: int;
  bg_threads: int;
  ticker_thread: bool;
}

let pp out self =
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
    bg_threads;
    ticker_thread;
  } =
    self
  in
  Format.fprintf out
    "{@[ debug=%B;@ url=%S;@ headers=%a;@ batch_traces=%a;@ batch_metrics=%a;@ \
     batch_logs=%a;@ batch_timeout_ms=%d; bg_threads=%d;@ ticker_thread=%B @]}"
    debug url ppheaders headers ppiopt batch_traces ppiopt batch_metrics ppiopt
    batch_logs batch_timeout_ms bg_threads ticker_thread

let make ?(debug = !debug_) ?(url = get_url ()) ?(headers = get_headers ())
    ?(batch_traces = Some 400) ?(batch_metrics = None) ?(batch_logs = Some 400)
    ?(batch_timeout_ms = 500) ?(thread = true) ?bg_threads
    ?(ticker_thread = true) () : t =
  let bg_threads =
    match bg_threads with
    | Some n -> max n 0
    | None ->
      if thread then
        4
      else
        0
  in
  {
    debug;
    url;
    headers;
    batch_traces;
    batch_metrics;
    batch_timeout_ms;
    batch_logs;
    bg_threads;
    ticker_thread;
  }
