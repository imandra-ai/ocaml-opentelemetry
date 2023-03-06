
## 0.4

- expose `Scope.get_surrounding`
- rehault of GC metrics
- `Trace.with_` now has `force_new_trace_id` param
- use thread-local storage to store global scope
- add `Span_link` module
- add a `Globals.default_span_kind` ref to specify default span kind

- fix(otel-lwt): missing modules now re-exported
- fix(client-ocurl): fix `tick` version used in the absence of bg thread

- drop dep on ocaml-protoc as the generated code is checked-in
- update vendored otel to 0.19

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
