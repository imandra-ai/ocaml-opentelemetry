
## 0.3

- improve error reporting from ocurl exporter
- improve GC sample collection
- feat(ocurl): simpler, cleaner backend implementation, with graceful exit
- config: make `Config.t` private (breaking)

## 0.2

- require pbrt only, use ocaml-protoc as a lint
- add `Metrics_callbacks` module
- add histogram metrics (untested)
- basic support for logs
- expose `tick` function (useful in the absence of a background thread)
- debug the library is set via `OTEL_OCAML_DEBUG`
- provide conventions for standard metrics
- add runtime attrs to GC stats
