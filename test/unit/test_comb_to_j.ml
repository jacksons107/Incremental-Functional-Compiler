open Compiler
open Comb
open J_machine
open Comb_to_j

let instr = Alcotest.testable pp_instr ( = )

let check name c expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check (Alcotest.list instr) name expected (comb_to_j c))

let suite =
  [
    check "literal instructions map straight through" (CInt 5) [ INT 5 ];
    check "bool literal" (CBool true) [ BOOL true ];
    check "constructor/unpack map straight through"
      (CConstr ("Pair", 2))
      [ CONSTR (2, "Pair") ];
    check "CUnpack maps to the 2-arity UNPACK global" CUnpack
      [ GLOBAL (2, UNPACK) ];
    (* comb_to_j emits e2's instructions, then e1's, then APP -- arguments
       are pushed onto the stack before the function, so naively "forward"
       order would silently reverse arguments at runtime. This is the one
       spot in codegen most worth pinning down with a regression test. *)
    check "CApp emits arg-then-fn-then-APP, not fn-then-arg"
      (CApp (CInt 1, CInt 2))
      [ INT 2; INT 1; APP ];
    check "argument order holds recursively through nested CApp"
      (CApp (CApp (CPlus, CInt 1), CInt 2))
      [ INT 2; INT 1; GLOBAL (2, ADD); APP; APP ];
    check "3-arity builtin (CIf) nested application"
      (CApp (CApp (CApp (CIf, CBool true), CInt 1), CInt 2))
      [ INT 2; INT 1; BOOL true; GLOBAL (3, IF); APP; APP; APP ];
    check
      "a free CVar compiles to an unresolved ID, not a compiler error (names \
       are resolved later, at assembly time)"
      (CVar "x") [ ID "x" ];
  ]
