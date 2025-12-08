open Common_
open Proto.Trace

type t = span_event

let make ?(time_unix_nano = Timestamp_ns.now_unix_ns ()) ?(attrs = [])
    (name : string) : t =
  let attrs = List.map Key_value.conv attrs in
  make_span_event ~time_unix_nano ~name ~attributes:attrs ()
