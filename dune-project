(lang dune 3.11)
(generate_opam_files true)
(package
  (name hachis)
  (authors
    "François Pottier <francois.pottier@inria.fr>"
  )
  (maintainers
    "François Pottier <francois.pottier@inria.fr>"
  )
  (source (uri git+https://github.com/fpottier/hachis.git))
  (homepage https://github.com/fpottier/hachis/)
  (bug_reports https://github.com/fpottier/hachis/issues)
  (synopsis "Hash sets and hash maps")
  (license "LGPL-2.0-only WITH OCaml-LGPL-linking-exception")
  (depends
    (ocaml (>= 4.14))
    (cppo (>= 1.7.0))
    (monolith :with-test)
    (ocamlfind :with-test)
  )
)
