(lang dune 2.7)

(name reparse)

(version 1.0.2)

(generate_opam_files true)

(source
 (github lemaetech/reparse))

(license MPL-2.0)

(authors "Bikal Lem <gbikal@gmail.com>")

(maintainers "Bikal Lem")

(package
 (name reparse)
 (synopsis "Recursive descent parsing library for ocaml")
 (description
   "reparse is an easy to learn and use parser combinator library. It is designed to aid authoring recursive descent style parsers.")
 (depends
  (ocaml
   (>= 4.10.0))
  dune
  (alcotest :with-test)))
