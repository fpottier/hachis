(lang dune 3.11)
(generate_opam_files true)
(package
  (name hachis)
  (authors
    "François Pottier"
  )
  (maintainers
    "François Pottier"
  )
  (source (uri git+https://github.com/fpottier/hachis.git))
  (homepage https://github.com/fpottier/hachis/)
  (bug_reports https://github.com/fpottier/hachis/issues)
  (synopsis "Hash sets and hash maps")
  (license "LGPL-2.0-only WITH OCaml-LGPL-linking-exception")
  (depends
    (ocaml (>= 4.14))
    (monolith :with-test)
    (ocamlfind :with-test)
  )
)
