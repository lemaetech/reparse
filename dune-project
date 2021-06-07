(lang dune 2.7)

(generate_opam_files false)

(name reparse)

(version 2.1.0)

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
  (ppx_expect :with-test)
  (mdx :with-test)))
(using mdx 0.1)
