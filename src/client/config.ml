
open Common_

type t = {
  debug: bool;
  url: string;
  batch_traces: int option;
  batch_metrics: int option;
  batch_timeout_ms: int;
  thread: bool;
  ticker_thread: bool;
}

let pp out self =
  let ppiopt = Format.pp_print_option Format.pp_print_int in
  let {debug; url; batch_traces; batch_metrics;
       batch_timeout_ms; thread; ticker_thread} = self in
  Format.fprintf out
    "{@[ debug=%B;@ url=%S;@ \
     batch_traces=%a;@ batch_metrics=%a;@ \
     batch_timeout_ms=%d; thread=%B;@ ticker_thread=%B @]}"
    debug url ppiopt batch_traces ppiopt batch_metrics
    batch_timeout_ms thread ticker_thread

let make
    ?(debug= !debug_)
    ?(url= get_url())
    ?(batch_traces=Some 400)
    ?(batch_metrics=None)
    ?(batch_timeout_ms=500)
    ?(thread=true)
    ?(ticker_thread=true)
    () : t =
  { debug; url; batch_traces; batch_metrics; batch_timeout_ms;
    thread; ticker_thread; }
