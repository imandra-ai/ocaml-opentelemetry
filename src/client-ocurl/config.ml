open Common_

type t = {
  debug: bool;
  url: string;
  headers: (string * string) list;
  batch_timeout_ms: int;
  bg_threads: int;
}

let pp out self =
  let pp_header ppf (a, b) = Format.fprintf ppf "@[%s: @,%s@]@." a b in
  let ppheaders = Format.pp_print_list pp_header in
  let { debug; url; headers; batch_timeout_ms; bg_threads } = self in
  Format.fprintf out
    "{@[ debug=%B;@ url=%S;@ headers=%a;@ batch_timeout_ms=%d; \
     bg_threads=%d;@]}"
    debug url ppheaders headers batch_timeout_ms bg_threads

let make ?(debug = !debug_) ?(url = get_url ()) ?(headers = get_headers ())
    ?(batch_timeout_ms = 500) ?bg_threads () : t =
  let bg_threads =
    match bg_threads with
    | Some n -> max n 2
    | None -> 4
  in
  { debug; url; headers; batch_timeout_ms; bg_threads }
