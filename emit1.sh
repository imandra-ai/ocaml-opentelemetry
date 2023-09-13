#!/bin/sh
OPTS="--profile=release --display=quiet --ignore-promoted-rules"
exec dune exec $OPTS tests/bin/emit1.exe -- $@
