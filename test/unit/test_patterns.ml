open Compiler
open Ast
open Patterns

let exp = Alcotest.testable Ast.pp_exp ( = )
let compile scruts rows = compile_match (Scruts scruts) (Matrix rows)

let check name scruts rows expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check exp name expected (compile scruts rows))

let suite =
  [
    check "all-var row compiles directly via gen_bindings, no If" [ Var "s" ]
      [ ([ PVar "x" ], Var "x") ]
      (Let ("x", Var "s", Var "x"));
    check "int-literal branch with a var default" [ Var "s" ]
      [ ([ PInt 1 ], Int 100); ([ PVar "y" ], Var "y") ]
      (If (Eq (Int 1, Var "s"), Int 100, Var "y"));
    (* Partition buckets non-var rows into a TagMap keyed by pat_tag, and
       TagMap.bindings returns entries in ascending key order regardless of
       the order the rows appeared in the source matrix. pat_tag's
       declaration order makes TagBool false < TagBool true, so the compiled
       tree always tests False before True -- even though this matrix lists
       the True row first. *)
    check
      "bool-literal branches with no default: Fail on exhaustion, tested \
       False-then-True regardless of source row order"
      [ Var "s" ]
      [ ([ PBool true ], Int 1); ([ PBool false ], Int 0) ]
      (If
         ( Eq (Bool false, Var "s"),
           Int 0,
           If (Eq (Bool true, Var "s"), Int 1, Fail) ));
    check "cons pattern rebinds head/tail via expand_cons" [ Var "s" ]
      [ ([ PCons (PVar "h", PVar "t") ], Var "h") ]
      (If
         ( IsCons (Var "s"),
           Let ("h", Head (Var "s"), Let ("t", Tail (Var "s"), Var "h")),
           Fail ));
    (* Same TagMap-ordering subtlety as the bool case: TagCons is declared
       before TagEmpty in pat_tag, so the Cons branch is always tested first
       even though this matrix lists the Empty row first. *)
    check
      "empty-vs-cons (list) match: Cons tested before Empty regardless of \
       source row order"
      [ Var "s" ]
      [ ([ PEmpty ], Int 0); ([ PCons (PVar "h", PVar "t") ], Var "h") ]
      (If
         ( IsCons (Var "s"),
           Let ("h", Head (Var "s"), Let ("t", Tail (Var "s"), Var "h")),
           If (Eq (Empty, Var "s"), Int 0, Fail) ));
    check "constructor pattern with bound args unpacks by declared arity order"
      [ Var "s" ]
      [
        ([ PConstr ("Pair", [ PVar "a"; PVar "b" ]) ], Plus (Var "a", Var "b"));
      ]
      (If
         ( IsConstr (Var "s", "Pair"),
           Let
             ( "a",
               Unpack ("Pair", Var "s", 0),
               Let ("b", Unpack ("Pair", Var "s", 1), Plus (Var "a", Var "b"))
             ),
           Fail ));
    check
      "multi-column (2-scrutinee) matrix: remaining scrutinees are threaded \
       through correctly after dropping the tested column"
      [ Var "s1"; Var "s2" ]
      [ ([ PInt 1; PVar "y" ], Var "y") ]
      (If (Eq (Int 1, Var "s1"), Let ("y", Var "s2", Var "y"), Fail));
    (* `partition` can never itself produce two default-bucket entries (the
       default map is keyed solely by TagVar, so it holds at most one
       binding) -- this exercises gen_tests's own defensive guard directly,
       bypassing partition, by handing it an artificial Partition value. *)
    Alcotest.test_case
      "gen_tests raises on multiple default-bucket entries (unreachable via \
       partition, but a real guard in gen_tests itself)"
      `Quick (fun () ->
        let m = Matrix [ ([ PVar "x" ], Var "x") ] in
        Alcotest.check_raises "multiple defaults"
          (Failure "Multiple defaults not yet supported") (fun () ->
            ignore
              (gen_tests (Scruts [ Var "s" ])
                 (Partition [], Partition [ (TagVar, m); (TagVar, m) ]))));
  ]
