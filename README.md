Reparse
=======

Reparse is an easy to learn and use parser combinator library for ocaml. It is 
designed to aid authoring recursive descent style parsers. It removes the
tedium of having to maintain parser/lexer input buffer. It emphasises and
enables monadic style of writing parsers. As such the parser uses an error
type to denote errors in parsing rather than an exception. 

Requirements
------------
Reparse requires ocaml version 4.10.0.

Installation
-----------
```sh
$ opam install reparse
```

API documentation
-----------------

```sh
$ opam install odig 
$ odig doc reparse 
```

Examples
--------
See https://github.com/lemaetech/http-mutlipart-formdata/blob/master/lib/http_multipart_formdata.ml 

References
----------
API function names broadly follow Angstrom library names.
