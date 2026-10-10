open Compiler
open Ast
open Dep_order

let names = Alcotest.(list string)

let check_refs name bound e expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check names name expected (global_refs bound e))

let defs_of src =
  let (Prog (defs, _)) = Driver.parse "test.oj" src in
  defs

let check_order name src expected =
  Alcotest.test_case name `Quick (fun () ->
      let result = topo_sort (defs_of src) in
      Alcotest.check names name expected (List.concat_map def_provides result))

let refs_suite =
  [
    check_refs "a free var is a reference" [] (Var "x") [ "x" ];
    check_refs "a bound var is not a reference" [ "x" ] (Var "x") [];
    check_refs "a let's own name is bound only in its continuation, not its \
                own body"
      [] (Let ("y", Int 1, Var "y")) [];
    check_refs "a let's bound expression is checked before its name is in \
                scope"
      [] (Let ("y", Var "z", Var "y")) [ "z" ];
    check_refs "a constructor name is always a reference, alongside its \
                argument references"
      []
      (Pack ("Cons", [ Var "h"; Var "t" ]))
      [ "Cons"; "h"; "t" ];
    check_refs "defrec's own name and params are bound within its own body, \
                matching the Y-combinator closing"
      []
      (Defrec ("f", [ "x" ], App (Var "f", Var "x"), Int 0))
      [];
    check_refs "match-pattern variables are bound within that case's rhs" []
      (Match
         ( [ Var "lst" ],
           [ ([ PCons (PVar "h", PVar "t") ], App (Var "h", Var "t")) ] ))
      [ "lst" ];
  ]

let order_suite =
  [
    check_order "already-ordered defs are returned unchanged"
      "let a = 1;\nlet b = a + 1;\na + b" [ "a"; "b" ];
    check_order "a forward reference is reordered before its dependent"
      "def foo x = bar x;\ndef bar x = x + 1;\nfoo 2" [ "bar"; "foo" ];
    check_order "defrec's self-reference isn't a dependency -- it neither \
                 reorders nor raises"
      "defrec fact n = if n == 0 then 1 else n + fact (n + -1);\nfact 5"
      [ "fact" ];
    Alcotest.test_case
      "a later def's use of a constructor reorders the DType ahead of it"
      `Quick (fun () ->
        let result =
          topo_sort
            (defs_of "def mk a b = Pair (a, b);\ntype pair = Pair of int * int;")
        in
        Alcotest.check names "order"
          [ "Pair"; "mk" ]
          (List.concat_map def_provides result));
    Alcotest.test_case
      "a genuine cycle between two separately-named defs raises \
       Mutual_recursion"
      `Quick (fun () ->
        Alcotest.check_raises "cycle"
          (Mutual_recursion [ "foo"; "bar"; "foo" ]) (fun () ->
            ignore
              (topo_sort
                 (defs_of "def foo x = bar x;\ndef bar x = foo x;\nfoo 2"))));
  ]

let suite = refs_suite @ order_suite
