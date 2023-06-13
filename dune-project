(lang dune 2.7)
(name opentelemetry)
(generate_opam_files true)
(source
 (github imandra-ai/ocaml-opentelemetry))

(version 0.4)

(authors "the Imandra team and contributors")
(maintainers "the Imandra team and contributors")
(license MIT)
;(documentation https://url/to/documentation)

(package
 (name opentelemetry)
 (synopsis "Instrumentation for https://opentelemetry.io")
 (depends
   (ocaml (>= "4.08"))
   ptime
   (odoc :with-doc)
   (pbrt (>= 2.3)))
 (depopts
   (trace (>= 0.1)))
 (tags
  (instrumentation tracing opentelemetry datadog jaeger)))

(package
 (name opentelemetry-lwt)
 (synopsis "Lwt-compatible instrumentation for https://opentelemetry.io")
 (depends
   (ocaml (>= "4.08"))
   (opentelemetry (= :version))
   (cohttp-lwt-unix :with-test)
   (odoc :with-doc)
   (lwt (>= "5.3"))
   lwt_ppx)
 (tags
  (instrumentation tracing opentelemetry datadog lwt)))

(package
  (name opentelemetry-client-ocurl)
  (depends
   (ocaml (>= "4.08"))
   (mtime (>= "1.4")) ; for spans
   ; atomic ; vendored
   (opentelemetry (= :version))
   (pbrt (>= 2.3))
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
  (lwt (>= "5.3"))
  (cohttp-lwt (>= "4.0.0")))
 (synopsis "Opentelemetry tracing for Cohttp HTTP servers"))

(package
  (name opentelemetry-client-cohttp-lwt)
  (depends
   (ocaml (>= "4.08"))
   (mtime (>= "1.4")) ; for spans
   (opentelemetry (= :version))
   (pbrt (>= 2.2))
   (odoc :with-doc)
   (lwt (>= "5.3"))
   lwt_ppx
   cohttp-lwt
   cohttp-lwt-unix)
  (synopsis "Collector client for opentelemetry, using cohttp + lwt"))
