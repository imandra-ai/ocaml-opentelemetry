#!/bin/sh
exec dune exec --profile=release tests/bin/emit1.exe -- $@
