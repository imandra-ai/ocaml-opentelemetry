## Release v0.17.0

* Add a test that `Sexp.to_string` works on large input.

* Improve error messages produced by `Sexp_conv`

* Use `[@tail_mod_cons]` in `sexp_of_list`.

* Add support for labeled tuples, a compiler extension available at:
  https://github.com/ocaml-flambda/flambda-backend

## Release v0.16.0

* Added `Sexp_conv_record`. Supports improvements to `ppx_sexp_conv` for deriving
  `of_sexp` on record types. Provides a GADT-based generic interface to parsing record
  sexps. This avoids having to generate the same field-parsing code over and over.

* Added `sexp_grammar_with_tags` and `sexp_grammar_with_tag_list` to `Sexp_conv_grammar`.
