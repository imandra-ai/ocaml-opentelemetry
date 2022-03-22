FROM ocaml/opam:debian-10-ocaml-4.12 AS build
WORKDIR /home/opam/ocaml-opentelemetry
RUN sudo apt-get update && sudo apt-get install pkg-config libcurl4-gnutls-dev -y
COPY ./opentelemetry-client-ocurl.opam ./
COPY ./opentelemetry-lwt.opam ./
COPY ./opentelemetry.opam ./
RUN opam update && opam install . --deps-only -y
COPY ./dune ./dune
COPY ./dune-project ./dune-project
COPY ./emit1.sh ./emit1.sh
COPY ./src ./src
COPY ./tests ./tests
COPY ./vendor/atomic ./vendor/atomic
COPY ./vendor/opentelemetry-proto/opentelemetry/proto ./vendor/opentelemetry-proto/opentelemetry/proto
RUN opam exec -- dune build @all --profile=release
CMD ["./emit1.sh"]

FROM debian:10 as run
RUN apt-get update && apt-get install libcurl4-gnutls-dev -y
COPY --from=build /home/opam/ocaml-opentelemetry/_build/default/tests/emit1.exe /emit1.exe
ENTRYPOINT ["/emit1.exe"]
