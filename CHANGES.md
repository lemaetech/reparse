## v3.1.0 2021-07-02

- Add `unsafe_take_cstruct` parser.
- `PARSER.parse` function now takes `?pos` parameter and returns `pos` in a tuple. This is implemented to allow creating pull based parsers.

## v3.0.1 2021-06-29

- Fix `take_while`

## v3.0.0 2021-06-23 UK

- Overhaul parser implementation - use functor based implementation. Introduce, `Make_buffered_input`, `Make_unbuffered_input` and `Make` functors. 
- Remove `reparse-unix` package
- Remove base dependency
- Facilitate IO promise monads such as `Lwt` and `Async`
- Add package `reparse-lwt` which defines `Lwt_stream.t` as one of the input sources.
- Add package 'reparse-lwt-unix' which defines `Lwt_unix.file_descr` and `Lwt_io.input_channel` as parser input sources.

## v2.1.0 2021-04-06 UK

This release has backwards incompatible changes.

- Infix functions are now available in `Parser` module itself.
- Add support for let operators - `let+`, `let*` ,`and+` and `and*`.
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
- Deprecate `Parser` module. Use `Reparse` instead.

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
