(lang dune 2.3)
(name opentelemetry)
(generate_opam_files true)
(source
 (github aestheticintegration/ocaml-opentelemetry))

(authors "the Imandra team")
(maintainers "the Imandra team")
(license MIT)
;(documentation https://url/to/documentation)

(package
 (name opentelemetry)
 (synopsis "Instrumentation for https://opentelemetry.io")
 (depends
   (ocaml (>= "4.08"))
   (dune (>= "2.3"))
   (ocaml-protoc (>= 2.1)))
 (depopts
   ptime)
 (tags
  (instrumentation tracing opentelemetry datadog jaeger)))

(package
  (name opentelemetry-client-ocurl)
  (depends
   (ocaml (>= "4.08"))
   (dune (>= "2.3"))
   (opentelemetry (= :version))
   (ocaml-protoc (>= 2.1))
   ocurl)
  (synopsis "Collector client for opentelemetry, using http + ocurl"))
