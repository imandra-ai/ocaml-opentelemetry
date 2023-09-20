(lang dune 2.7)

(name opentelemetry)

(generate_opam_files true)

(source
 (github imandra-ai/ocaml-opentelemetry))

(version 0.5)

(authors "the Imandra team and contributors")

(maintainers "the Imandra team and contributors")

(license MIT)

;(documentation https://url/to/documentation)

(package
 (name opentelemetry)
 (synopsis "Instrumentation for https://opentelemetry.io")
 (depends
  (ocaml
   (>= "4.08"))
  ptime
  ambient-context
  (odoc :with-doc)
  (alcotest :with-test)
  (pbrt
   (>= 2.3))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    :with-dev-setup
    (>= 0.24)
    (< 0.25))))
 (depopts trace)
 (conflicts
  (trace
   (or
    (< 0.4)
    (>= 0.5))))
 (tags
  (instrumentation tracing opentelemetry datadog jaeger)))

(package
 (name opentelemetry-lwt)
 (synopsis "Lwt-compatible instrumentation for https://opentelemetry.io")
 (depends
  (ocaml
   (>= "4.08"))
  ambient-context
  (opentelemetry
   (= :version))
  (cohttp-lwt-unix :with-test)
  (odoc :with-doc)
  (lwt
   (>= "5.3"))
  (lwt_ppx
   (>= "2.0")))
 (tags
  (instrumentation tracing opentelemetry datadog lwt)))

(package
 (name opentelemetry-client-ocurl)
 (depends
  (ocaml
   (>= "4.08"))
  (mtime
   (>= "1.4"))
  ; for spans
  ; atomic ; vendored
  (opentelemetry
   (= :version))
  (pbrt
   (>= 2.3))
  (odoc :with-doc)
  (ezcurl
   (>= 0.2.3))
  ocurl)
 (synopsis "Collector client for opentelemetry, using http + ezcurl"))

(package
 (name opentelemetry-cohttp-lwt)
 (depends
  (ocaml
   (>= "4.08"))
  (opentelemetry
   (= :version))
  (opentelemetry-lwt
   (= :version))
  (odoc :with-doc)
  (lwt
   (>= "5.3"))
  (cohttp-lwt
   (>= "4.0.0")))
 (synopsis "Opentelemetry tracing for Cohttp HTTP servers"))

(package
 (name opentelemetry-client-cohttp-lwt)
 (depends
  (ocaml
   (>= "4.08"))
  (mtime
   (>= "1.4"))
  ; for spans
  (opentelemetry
   (= :version))
  (pbrt
   (>= 2.2))
  (odoc :with-doc)
  (lwt
   (>= "5.3"))
  (lwt_ppx
   (>= "2.0"))
  cohttp-lwt
  cohttp-lwt-unix)
 (synopsis "Collector client for opentelemetry, using cohttp + lwt"))
