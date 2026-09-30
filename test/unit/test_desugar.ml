open Compiler
open Ast
open Desugar

let exp = Alcotest.testable Ast.pp_exp ( = )

let typedef_pp fmt (TypeDef (t, c, args)) =
  Format.fprintf fmt "TypeDef(%s, %s, [%a])" t c
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       Ast.pp_typ)
    args

let typedef = Alcotest.testable typedef_pp ( = )

let check_def_to_exp name defs body expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check exp name expected (def_to_exp (Prog (defs, body))))

let check_get_types name defs expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check (Alcotest.list typedef) name expected (get_types defs))

let suite =
  [
    check_def_to_exp "empty def list returns the body unchanged" [] (Int 5)
      (Int 5);
    check_def_to_exp "single DLet becomes a Let"
      [ DLet ("x", Int 1) ]
      (Var "x")
      (Let ("x", Int 1, Var "x"));
    check_def_to_exp "single DDef becomes a Def"
      [ DDef ("f", [ "x" ], Var "x") ]
      (App (Var "f", Int 1))
      (Def ("f", [ "x" ], Var "x", App (Var "f", Int 1)));
    check_def_to_exp "single DDefrec becomes a Defrec"
      [ DDefrec ("f", [ "x" ], Var "x") ]
      (App (Var "f", Int 1))
      (Defrec ("f", [ "x" ], Var "x", App (Var "f", Int 1)));
    check_def_to_exp "mixed defs nest preserving order"
      [ DLet ("x", Int 1); DDef ("f", [ "y" ], Var "y") ]
      (App (Var "f", Var "x"))
      (Let ("x", Int 1, Def ("f", [ "y" ], Var "y", App (Var "f", Var "x"))));
    check_def_to_exp
      "DType with multiple constructors becomes a right-nested Constr chain in \
       declaration order"
      [ DType ("t", [ ("A", [ TInt ]); ("B", []) ]) ]
      (Var "x")
      (Constr ("A", [ TInt ], Constr ("B", [], Var "x")));
    check_get_types "no DType among defs yields []" [ DLet ("x", Int 1) ] [];
    check_get_types "one DType extracts its full constructor list"
      [ DType ("t", [ ("A", [ TInt ]); ("B", []) ]) ]
      [ TypeDef ("t", "A", [ TInt ]); TypeDef ("t", "B", []) ];
    check_get_types
      "DType interleaved with other defs: only typedefs are extracted"
      [
        DLet ("x", Int 1);
        DType ("t", [ ("A", []) ]);
        DDef ("f", [ "y" ], Var "y");
      ]
      [ TypeDef ("t", "A", []) ];
    check_get_types "multiple DTypes are concatenated preserving order"
      [ DType ("t1", [ ("A", []) ]); DType ("t2", [ ("B", [ TInt ]) ]) ]
      [ TypeDef ("t1", "A", []); TypeDef ("t2", "B", [ TInt ]) ];
  ]
