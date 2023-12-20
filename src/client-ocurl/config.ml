open Common_

type t = {
  debug: bool;
  url: string;
  headers: (string * string) list;
  batch_timeout_ms: int;
  bg_threads: int;
  ticker_thread: bool;
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
    self_trace;
  } =
    self
  in
  Format.fprintf out
    "{@[ debug=%B;@ url=%S;@ headers=%a;@ batch_timeout_ms=%d; bg_threads=%d;@ \
     ticker_thread=%B;@ self_trace=%B @]}"
    debug url ppheaders headers batch_timeout_ms bg_threads ticker_thread
    self_trace

let make ?(debug = !debug_) ?(url = get_url ()) ?(headers = get_headers ())
    ?(batch_timeout_ms = 500) ?(bg_threads = 4) ?(ticker_thread = true)
    ?(self_trace = true) () : t =
  let bg_threads = max 2 (min bg_threads 32) in
  {
    debug;
    url;
    headers;
    batch_timeout_ms;
    bg_threads;
    ticker_thread;
    self_trace;
  }
