(lang dune 2.7)

(name reparse)

(version 2.0.0)

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
   "Monadic, recursive descent based, parser construction library for ocaml. Comprehensively documented and tested.")
 (depends
  (ocaml
   (>= 4.10.0))
  (alcotest :with-test)))

(package
 (name reparse-unix)
 (synopsis
   "Provides support for parsing files as source of input for reparse library ")
 (description
   "Provides support for parsing files as source of input for reparse library ")
 (depends
  (ocaml
   (>= 4.10.0))
  (reparse
   (= 2.0.0))
  base-unix
  (alcotest :with-test)))
