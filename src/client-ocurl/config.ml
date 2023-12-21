open Common_

type t = {
  debug: bool;
  url: string;
  headers: (string * string) list;
  batch_timeout_ms: int;
  bg_threads: int;
  ticker_thread: bool;
  ticker_interval_ms: int;
  self_trace: bool;
}

let pp out self =
  let pp_header ppf (a, b) = Format.fprintf ppf "@[%s: @,%s@]@." a b in
  let ppheaders = Format.pp_print_list pp_header in
  let {
    debug;
    url;
    headers;
    batch_timeout_ms;
    bg_threads;
    ticker_thread;
    ticker_interval_ms;
    self_trace;
  } =
    self
  in
  Format.fprintf out
    "{@[ debug=%B;@ url=%S;@ headers=%a;@ batch_timeout_ms=%d; bg_threads=%d;@ \
     ticker_thread=%B;@ ticker_interval_ms=%d;@ self_trace=%B @]}"
    debug url ppheaders headers batch_timeout_ms bg_threads ticker_thread
    ticker_interval_ms self_trace

let make ?(debug = !debug_) ?(url = get_url ()) ?(headers = get_headers ())
    ?(batch_timeout_ms = 2_000) ?(bg_threads = 4) ?(ticker_thread = true)
    ?(ticker_interval_ms = 500) ?(self_trace = false) () : t =
  let bg_threads = max 1 (min bg_threads 32) in
  {
    debug;
    url;
    headers;
    batch_timeout_ms;
    bg_threads;
    ticker_thread;
    ticker_interval_ms;
    self_trace;
  }
