#!/bin/sh
exec dune exec --profile=release tests/emit1.exe -- $@
