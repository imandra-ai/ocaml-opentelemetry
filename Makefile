
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
