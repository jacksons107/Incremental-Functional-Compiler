(lang dune 3.19)
(using menhir 3.0)

(name compiler)

(generate_opam_files true)

(source
 (github jacksons107/Incremental-Functional-Compiler))

(authors "Jackson Slipock <jackson.slipock@gmail.com>")

(maintainers "Jackson Slipock <jackson.slipock@gmail.com>")

(package
 (name compiler)
 (synopsis "A compiler for a small ML-style functional language")
 (description
  "Compiles a small Hindley-Milner-typed functional language (let/def/defrec, pattern matching, algebraic data types, lists) to C, linking against a hand-written graph-reduction runtime with a copying garbage collector.")
 (depends
  ocaml
  (menhir
   (>= 20260209))
  (alcotest :with-test)
  (ppx_expect :with-test)
  (ppx_inline_test :with-test))
 (tags
  (compiler functional-programming ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
