open Compiler
open Lam
open Serialize

let i64be n =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 (Int64.of_int n);
  Bytes.to_string b

let tag n = String.make 1 (Char.chr n)

let check name expected e =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check string) name expected (encode ~free_map:no_free_vars e))

let distinct_tags_test () =
  let samples =
    [
      LInt 0;
      LBool false;
      LString "";
      LEq;
      LPlus;
      LIf;
      LHead;
      LTail;
      LCons;
      LEmpty;
      LConstr ("c", 0);
      LUnpack;
      LIsCons;
      LIsConstr;
      LFail;
      LY;
    ]
  in
  let tags =
    List.map (fun e -> (encode ~free_map:no_free_vars e).[0]) samples
  in
  let uniq = List.sort_uniq Char.compare tags in
  Alcotest.(check int)
    "distinct tag per non-binding constructor" (List.length samples)
    (List.length uniq)

let determinism_test () =
  let e = LApp (LConstr ("Pair", 2), LApp (LInt 42, LBool true)) in
  Alcotest.(check string)
    "encode is deterministic"
    (encode ~free_map:no_free_vars e)
    (encode ~free_map:no_free_vars e)

let bound_depth_test () =
  Alcotest.(check string)
    "innermost binder's own var is depth 0"
    (tag 18 ^ tag 0 ^ i64be 0)
    (encode ~free_map:no_free_vars (Lam ("x", LVar "x")));
  Alcotest.(check string)
    "a var bound two lambdas out is depth 1"
    (tag 18 ^ tag 18 ^ tag 0 ^ i64be 1)
    (encode ~free_map:no_free_vars (Lam ("x", Lam ("y", LVar "x"))));
  Alcotest.(check string)
    "the innermost lambda's own var is still depth 0, regardless of outer \
     binders"
    (tag 18 ^ tag 18 ^ tag 0 ^ i64be 0)
    (encode ~free_map:no_free_vars (Lam ("x", Lam ("y", LVar "y"))))

let shadowing_test () =
  Alcotest.(check string)
    "an inner binder reusing the outer binder's name shadows it -- the var \
     resolves to the inner (depth 0), not the outer"
    (tag 18 ^ tag 18 ^ tag 0 ^ i64be 0)
    (encode ~free_map:no_free_vars (Lam ("x", Lam ("x", LVar "x"))))

let alpha_equivalence_test () =
  Alcotest.(check string)
    "alpha-equivalent lambdas encode to identical bytes, since only de Bruijn \
     depth is serialized, never the binder's name"
    (encode ~free_map:no_free_vars (Lam ("x", LVar "x")))
    (encode ~free_map:no_free_vars (Lam ("y", LVar "y")));
  Alcotest.(check string)
    "alpha-equivalence holds for nested binders too"
    (encode ~free_map:no_free_vars
       (Lam ("x", Lam ("y", LApp (LVar "x", LVar "y")))))
    (encode ~free_map:no_free_vars
       (Lam ("a", Lam ("b", LApp (LVar "a", LVar "b")))))

let free_variable_test () =
  let free_map x = List.assoc x [ ("foo", "HASH1"); ("bar", "H2") ] in
  Alcotest.(check string)
    "a free var emits its resolved hash bytes"
    (tag 19 ^ i64be 5 ^ "HASH1")
    (encode ~free_map (LVar "foo"));
  Alcotest.(check string)
    "a bound var and a free var in the same term resolve independently"
    (tag 18 ^ tag 17 ^ tag 0 ^ i64be 0 ^ tag 19 ^ i64be 2 ^ "H2")
    (encode ~free_map (Lam ("x", LApp (LVar "x", LVar "bar"))))

let unbound_free_variable_raises_test () =
  Alcotest.check_raises "a free var with no_free_vars raises"
    (Serialize.Unbound_free_variable "z") (fun () ->
      ignore (encode ~free_map:no_free_vars (LVar "z")))

let suite =
  [
    check "LInt" (tag 1 ^ i64be 5) (LInt 5);
    check "LInt negative" (tag 1 ^ i64be (-3)) (LInt (-3));
    check "LBool true" (tag 2 ^ "\001") (LBool true);
    check "LBool false" (tag 2 ^ "\000") (LBool false);
    check "LString" (tag 3 ^ i64be 3 ^ "abc") (LString "abc");
    check "LString empty" (tag 3 ^ i64be 0 ^ "") (LString "");
    check "LEq" (tag 4) LEq;
    check "LPlus" (tag 5) LPlus;
    check "LIf" (tag 6) LIf;
    check "LHead" (tag 7) LHead;
    check "LTail" (tag 8) LTail;
    check "LCons" (tag 9) LCons;
    check "LEmpty" (tag 10) LEmpty;
    check "LConstr" (tag 11 ^ i64be 4 ^ "Cons" ^ i64be 2) (LConstr ("Cons", 2));
    check "LUnpack" (tag 12) LUnpack;
    check "LIsCons" (tag 13) LIsCons;
    check "LIsConstr" (tag 14) LIsConstr;
    check "LFail" (tag 15) LFail;
    check "LY" (tag 16) LY;
    check "LApp" (tag 17 ^ tag 1 ^ i64be 1 ^ tag 5) (LApp (LInt 1, LPlus));
    check "LApp nested"
      (tag 17 ^ (tag 17 ^ tag 5 ^ tag 6) ^ tag 9)
      (LApp (LApp (LPlus, LIf), LCons));
    Alcotest.test_case "distinct tags" `Quick distinct_tags_test;
    Alcotest.test_case "deterministic" `Quick determinism_test;
    Alcotest.test_case "bound variable depth" `Quick bound_depth_test;
    Alcotest.test_case "shadowing resolves to the inner binder" `Quick
      shadowing_test;
    Alcotest.test_case "alpha-equivalence" `Quick alpha_equivalence_test;
    Alcotest.test_case "free variable resolution" `Quick free_variable_test;
    Alcotest.test_case "unbound free variable raises" `Quick
      unbound_free_variable_raises_test;
  ]
