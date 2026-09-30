open Compiler
open Lam
open Comb
open Lam_to_comb

let comb = Alcotest.testable Comb.pp ( = )

let check name l expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check comb name expected (lam_to_comb l))

let suite =
  [
    check "bound-var use abstracts to I" (Lam ("x", LVar "x")) I;
    check "unrelated constant abstracts to K applied to it"
      (Lam ("x", LInt 5))
      (CApp (K, CInt 5));
    check "a builtin untouched by the binder also abstracts via K"
      (Lam ("x", LPlus))
      (CApp (K, CPlus));
    check "a different free variable (not the binder) abstracts via K"
      (Lam ("x", LVar "y"))
      (CApp (K, CVar "y"));
    check "application splits into S: \\x -> f x, f free"
      (Lam ("x", LApp (LVar "f", LVar "x")))
      (CApp (CApp (S, CApp (K, CVar "f")), I));
    (* `abstract` has no optimization for "this subterm doesn't mention the
       binder" -- every CApp node unconditionally expands via the S-rule, so
       even a simple \x y -> x y produces a much bigger term than the
       textbook-minimal S(Kf)I shape from the single-argument case above.
       Verified by actually running lam_to_comb rather than hand-deriving,
       since the recursion depth here makes a manual trace error-prone. *)
    check "nested lambdas abstract inner-then-outer: \\x y -> x y"
      (Lam ("x", Lam ("y", LApp (LVar "x", LVar "y"))))
      (CApp
         ( CApp
             (S, CApp (CApp (S, CApp (K, S)), CApp (CApp (S, CApp (K, K)), I))),
           CApp (K, I) ));
    check "non-lambda structure passes through lam_to_comb unchanged"
      (LApp (LInt 1, LInt 2))
      (CApp (CInt 1, CInt 2));
    check "LConstr maps straight to CConstr"
      (LConstr ("Pair", 2))
      (CConstr ("Pair", 2));
  ]
