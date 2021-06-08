(lang dune 2.8)

(generate_opam_files true)

(name reparse)

(version 3.0.0)

(using mdx 0.1)

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

(package
 (name reparse-lwt-unix)
 (synopsis "lwt-unix support for reparse")
 (description "Lwt_stream.t and Lwt_io.input_channel support for 'reparse'.")
 (depends
  (ocaml
   (>= 4.10.0))
  (lwt
   (>= 5.4.1))))
