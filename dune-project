(lang dune 2.7)
(name opentelemetry)
(generate_opam_files true)
(source
 (github aestheticintegration/ocaml-opentelemetry))

(version 0.1)

(authors "the Imandra team")
(maintainers "the Imandra team")
(license MIT)
;(documentation https://url/to/documentation)

(package
 (name opentelemetry)
 (synopsis "Instrumentation for https://opentelemetry.io")
 (depends
   (ocaml (>= "4.08"))
   ptime
   (odoc :with-doc)
   (ocaml-protoc (>= 2.1)))
 (tags
  (instrumentation tracing opentelemetry datadog jaeger)))

(package
 (name opentelemetry-lwt)
 (synopsis "Lwt-compatible instrumentation for https://opentelemetry.io")
 (depends
   (ocaml (>= "4.08"))
   (opentelemetry (= :version))
   (odoc :with-doc)
   lwt)
 (tags
  (instrumentation tracing opentelemetry datadog lwt)))

(package
  (name opentelemetry-client-ocurl)
  (depends
   (ocaml (>= "4.08"))
   (mtime (>= "1.4")) ; for spans
   ; atomic ; vendored
   (opentelemetry (= :version))
   (ocaml-protoc (>= 2.1))
   (odoc :with-doc)
   ocurl)
  (synopsis "Collector client for opentelemetry, using http + ocurl"))

(package
 (name opentelemetry-cohttp-lwt)
 (depends
  (ocaml (>= "4.08"))
  (opentelemetry (= :version))
  (opentelemetry-lwt (= :version))
  (odoc :with-doc)
  cohttp-lwt)
 (synopsis "Opentelemetry tracing for Cohttp HTTP servers"))
