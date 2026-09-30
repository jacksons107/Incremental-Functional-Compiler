open Compiler
open Ast

let prog = Alcotest.testable Ast.pp_prog ( = )

(* Bypasses Driver.parse deliberately: Driver.parse calls `exit 1` on a syntax
   error, which would kill the test runner instead of letting us assert on
   [Parser.Error]. *)
let parse s = Parser.prog Lexer.read (Lexing.from_string s)

let check name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check prog name expected (parse input))

let check_syntax_error name input =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check_raises name Parser.Error (fun () -> ignore (parse input)))

let suite =
  [
    check "int literal" "5" (Prog ([], Int 5));
    check "bool literal" "True" (Prog ([], Bool true));
    check "arithmetic is left-associative" "1 + 2 + 3"
      (Prog ([], Plus (Plus (Int 1, Int 2), Int 3)));
    check "equality" "1 == 2" (Prog ([], Eq (Int 1, Int 2)));
    check "if/then/else" "if True then 1 else 2"
      (Prog ([], If (Bool true, Int 1, Int 2)));
    check "top-level let (DLet)" "let x = 5;\nx"
      (Prog ([ DLet ("x", Int 5) ], Var "x"));
    check "local let-in, parenthesized" "(let x = 5 in x)"
      (Prog ([], Let ("x", Int 5, Var "x")));
    check "top-level def with args" "def add x y = x + y;\nadd 1 2"
      (Prog
         ( [ DDef ("add", [ "x"; "y" ], Plus (Var "x", Var "y")) ],
           App (App (Var "add", Int 1), Int 2) ));
    check "top-level defrec" "defrec fact n = n;\nfact 5"
      (Prog ([ DDefrec ("fact", [ "n" ], Var "n") ], App (Var "fact", Int 5)));
    check "list literal" "[1, 2, 3]" (Prog ([], List [ Int 1; Int 2; Int 3 ]));
    check "empty list" "[]" (Prog ([], Empty));
    check "cons requires explicit parens to nest" "1 :: (2 :: [])"
      (Prog ([], Cons (Int 1, Cons (Int 2, Empty))));
    check "match over a cons pattern" "match [1,2] with | h::t -> h"
      (Prog
         ( [],
           Match
             ( [ List [ Int 1; Int 2 ] ],
               [ ([ PCons (PVar "h", PVar "t") ], Var "h") ] ) ));
    check "match over empty and cons patterns"
      "match [] with | [] -> 0 | h::t -> h"
      (Prog
         ( [],
           Match
             ( [ Empty ],
               [
                 ([ PEmpty ], Int 0); ([ PCons (PVar "h", PVar "t") ], Var "h");
               ] ) ));
    check "sum type declaration + constructor pattern with args"
      "type pair = Pair of int * int;\n\
       match Pair(1,2) with | Pair(x, y) -> x + y"
      (Prog
         ( [ DType ("pair", [ ("Pair", [ TInt; TInt ]) ]) ],
           Match
             ( [ Pack ("Pair", [ Int 1; Int 2 ]) ],
               [
                 ( [ PConstr ("Pair", [ PVar "x"; PVar "y" ]) ],
                   Plus (Var "x", Var "y") );
               ] ) ));
    check "multi-constructor sum type declaration"
      "type shape = Circle of int | Square of int;\n0"
      (Prog
         ( [ DType ("shape", [ ("Circle", [ TInt ]); ("Square", [ TInt ]) ]) ],
           Int 0 ));
    check "constructor field with postfix list type"
      "type box = Box of int list;\nBox([1,2,3])"
      (Prog
         ( [ DType ("box", [ ("Box", [ TList TInt ]) ]) ],
           Pack ("Box", [ List [ Int 1; Int 2; Int 3 ] ]) ));
    check "bare nullary constructor reference" "type nullary = Nil;\nNil"
      (Prog ([ DType ("nullary", [ ("Nil", []) ]) ], Pack ("Nil", [])));
    check "head builtin" "head [1,2]" (Prog ([], Head (List [ Int 1; Int 2 ])));
    check "tail builtin" "tail [1,2]" (Prog ([], Tail (List [ Int 1; Int 2 ])));
    check_syntax_error "malformed input raises Parser.Error" "let x = ;";
  ]
