(lang dune 2.6)

(name reparse)

(version 1.0.0)

(generate_opam_files true)

(source
 (github bikallem/reparse))

(license MPL-2.0)

(authors "Bikal Lem <gbikal@gmail.com>")

(maintainers "Bikal Lem")

(package
 (name reparse)
 (synopsis "monadic, recursive descent parsing library for ocaml.")
 (depends
  (ocaml
   (>= 4.10.0))
  (dune
   (and
    :build
    (>= 2.6.0)))
  sexplib
  ppx_sexp_conv
  bigstringaf))
