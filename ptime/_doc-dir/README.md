Ptime — POSIX time for OCaml
============================

Ptime has platform independent POSIX time support in pure OCaml. It
provides a type to represent a well-defined range of POSIX timestamps
with picosecond precision, conversion with date-time values,
conversion with [RFC 3339 timestamps][rfc3339] and pretty printing to
a human-readable, locale-independent representation.

The additional Ptime_clock library provides access to a system POSIX
clock and to the system's current time zone offset.

Ptime is not a calendar library.

Ptime has no dependency. Ptime_clock depends on your system library or
JavaScript runtime system. Ptime and its libraries are distributed
under the ISC license.

[rfc3339]: http://tools.ietf.org/html/rfc3339

Home page: <http://erratique.ch/software/ptime>  

# Installation

Ptime can be installed with `opam`:

    opam install ptime

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Documentation

The documentation can be consulted [online] or via `odig doc mtime`.

Questions are welcome but better asked on the [OCaml forum] than on
the issue tracker.

[online]: http://erratique.ch/software/ptime/doc/
[OCaml forum]: https://discuss.ocaml.org/

# Sample programs

See [test/min_clock.ml](test/min_clock.ml).

If you installed ptime with `opam` sample programs are located in
the directory `opam var ptime:doc`.
