#!/bin/sh
exec dune exec --profile=release tests/bin/emit1_stdout.exe -- $@
