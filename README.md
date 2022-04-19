
# Opentelemetry [![build](https://github.com/AestheticIntegration/ocaml-opentelemetry/actions/workflows/main.yml/badge.svg)](https://github.com/AestheticIntegration/ocaml-opentelemetry/actions/workflows/main.yml)

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

## Configuration

The library is configurable via `Opentelemetry.Config`, via the standard
opentelemetry env variables, or with some custom environment variables.

- `OTEL_EXPORTER_OTLP_ENDPOINT` sets the http endpoint to send signals to
- `OTEL_OCAML_DEBUG=1` to print some debug messages from the opentelemetry library ide
- `OTEL_RESOURCE_ATTRIBUTES` sets a comma separated list of custom resource attributes

## Collector opentelemetry-client-ocurl

This is a synchronous collector that uses the http+protobuf format
to send signals (metrics, traces) to some other collector (eg. `otelcol`
or the datadog agent).

## License

MIT

## Semantic Conventions

Not supported yet.

- [ ] [metrics](https://opentelemetry.io/docs/reference/specification/metrics/semantic_conventions/)
- [ ] [traces](https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/)
- [ ] [resources](https://opentelemetry.io/docs/reference/specification/resource/semantic_conventions/)
