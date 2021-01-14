## v3.0.0 2021-01-14

This release has backwards incompatible changes. 

- Deprecate `Infix` module. Infix functions are now available in `Parser`
  moduel itself.
- `sep_by` parameters in `take, take_while, take_while_cb` functions are now `unit t`.
- Add support for let operators `and+` and `and*`.
- `bind` and `map` function are now labelled following `base` library
  dependency convention.
- Items in `all_unit` are now `unit t` rather than `_ t` following monad
  combinator convention in `base` library dependency.
- `pure` is now deprecated. Use `return` instead. This is to stay consistent
  with monad conventions in `base` library dependency.
- `>|=` is deprecated. Use `>>|` instead. This is to stay consistent with monad
  conventions in `base` library dependency.
- Removed `map4` function.
- Add support for `ppx_let`.

## v2.0.0 2020-11-09 UK

- Rewrite the whole package to use exceptions rather than `result` type
- Adds many more parsing combinators
- Adds comprehensive unit tests
- Adds comprehensive documentation, host documentation and add links in repo home page
- Adds abstraction for input source
- Provides unix file source and string input source
- Adds separate package `reparse-unix` for unix file input
- Adds calc.ml and json.ml in examples.

## v1.2.0 2020-09-13 UK

- Remove Result and underlying monad
- Remove sexplib0, bigstringaf
- Prune basic parsers

## v1.0.2 2020-07-30 UK

- Add string_if parser.

## v1.0.1 2020-07-30 UK

- Add sexp_of_error.

## v1.0.0 2020-07-23 Reading, UK

- First release.
