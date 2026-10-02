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
      Alcotest.(check string) name expected (encode e))

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
  let tags = List.map (fun e -> (encode e).[0]) samples in
  let uniq = List.sort_uniq Char.compare tags in
  Alcotest.(check int)
    "distinct tag per non-binding constructor" (List.length samples)
    (List.length uniq)

let determinism_test () =
  let e = LApp (LConstr ("Pair", 2), LApp (LInt 42, LBool true)) in
  Alcotest.(check string) "encode is deterministic" (encode e) (encode e)

let binding_cases_not_implemented_test () =
  Alcotest.check_raises "LVar raises"
    (Failure "Serialize.write: LVar is a binding case, not yet implemented")
    (fun () -> ignore (encode (LVar "x")));
  Alcotest.check_raises "Lam raises"
    (Failure "Serialize.write: Lam is a binding case, not yet implemented")
    (fun () -> ignore (encode (Lam ("x", LVar "x"))))

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
    Alcotest.test_case "binding cases unimplemented" `Quick
      binding_cases_not_implemented_test;
  ]
