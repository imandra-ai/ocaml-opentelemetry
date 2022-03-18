
# Opentelemetry

This project provides an API for instrumenting server software
using [opentelemetry](https://opentelemetry.io/docs), as well as
connectors to talk to opentelemetry software such as [jaeger](https://www.jaegertracing.io/).

## Features

- [x] basic traces
- [x] basic metrics
- [ ] basic logs
- [ ] nice API
- [x] interface with `lwt`
- [x] sync collector relying on ocurl
  * [ ] batching, perf, etc.
- [ ] async collector relying on ocurl-multi
- [ ] interface with `logs` (carry context around)

## Use

For now, instrument manually:

```ocaml
module T = Opentelemetry
let (let@) f x = f x

let foo x =
  let@ (tr,sp) = T.Trace.with_ 
      ~service_name:"myservice" "foo" ~attrs:["hello", `String "world"] in
  (* … *)

  let gc = Gc.stat() in
  T.Metrics.(
    emit [
      gauge ~name:"foo.gc.major_heap_words" [int gc.Gc.heap_words];
      sum ~name:"foo.gc.minor_allocated" [float gc.Gc.minor_words];
    ]);

  (* … *)
``` 

## License

MIT

## Semantic Conventions

Not supported yet.

- [ ] [metrics](https://opentelemetry.io/docs/reference/specification/metrics/semantic_conventions/)
- [ ] [traces](https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/)
- [ ] [resources](https://opentelemetry.io/docs/reference/specification/resource/semantic_conventions/)
