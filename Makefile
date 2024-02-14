
OPTS=--profile=release --ignore-promoted-rules

all:
	@dune build @all $(OPTS)

test:
	@dune runtest --force $(OPTS)

clean:
	@dune clean

protoc-gen:
	@dune build @lint

format:
	@dune build @fmt

WATCH ?= @all
watch:
	@dune build $(WATCH) -w $(OPTS)

VERSION=$(shell awk '/^version:/ {print $$2}' opentelemetry.opam)
update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_VERSION/$(VERSION)/g" $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/**/*.ml) $(wildcard src/*.mli) $(wildcard src/**/*.mli)
