
OPTS=--profile=release
all:
	@dune build @all $(OPTS)

test:
	@dune runtest --force $(OPTS)

clean:
	@dune clean

WATCH ?= @all
watch:
	@dune build $(WATCH) -w $(OPTS)
