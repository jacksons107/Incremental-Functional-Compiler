open Compiler

(* Exercises Driver.typecheck: the full parse/desugar/elaborate/infer
   pipeline on a whole program, with no project involved. *)

let typ = Alcotest.testable Ast.pp_typ ( = )

(* infer can leave resolved type variables as [TVar {contents = Link ty}]
   rather than substituting [ty] back in; deep_prune collapses those links
   throughout a compound type so structural equality matches what pp_typ
   already displays. *)
let rec deep_prune ty =
  match Type_infer.prune ty with
  | Ast.TList t -> Ast.TList (deep_prune t)
  | Ast.TLam (a, b) -> Ast.TLam (deep_prune a, deep_prune b)
  | t -> t

let check name src expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check typ name (deep_prune expected)
        (deep_prune (Driver.typecheck ~filename:"test.oj" ~source:src)))

let check_raises_type_error name src =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check_raises name (Type_infer.Type_error "placeholder") (fun () ->
          try ignore (Driver.typecheck ~filename:"test.oj" ~source:src)
          with Type_infer.Type_error _ -> raise (Type_infer.Type_error "placeholder")))

let suite =
  [
    check "int literal" "5" Ast.TInt;
    check "bool literal" "True" Ast.TBool;
    check "arithmetic is left-associative" "1 + 2 + 3" Ast.TInt;
    check "equality" "1 == 2" Ast.TBool;
    check "if/then/else" "if True then 1 else 2" Ast.TInt;
    check "top-level let" "let x = 5;\nx" Ast.TInt;
    check "top-level def with args" "def add x y = x + y;\nadd 1 2" Ast.TInt;
    check "top-level defrec recurses to a base case"
      "defrec sum n = if n == 0 then 0 else n + sum (n + -1);\nsum 3"
      Ast.TInt;
    check "list literal via head" "head [1, 2, 3]" Ast.TInt;
    check "tail preserves the list's element type" "tail [1, 2]" (Ast.TList Ast.TInt);
    check "cons builds a list" "1 :: (2 :: [])" (Ast.TList Ast.TInt);
    check "match over a cons pattern" "match [1, 2] with | h::t -> h" Ast.TInt;
    check "match over empty and cons patterns"
      "match [] with | [] -> 0 | h::t -> h" Ast.TInt;
    check "sum type declaration + constructor pattern with args type-checks \
           end to end"
      "type pair = Pair of int * int;\n\
       match Pair (1, 2) with | Pair (x, y) -> x + y"
      Ast.TInt;
    check
      "a self-referential user-defined list type-checks end to end, \
       including pattern matching its recursive field"
      "type intlist = Nil | Cons of int * intlist;\n\
       let xs = Cons (1, Cons (2, (Nil)));\n\
       match xs with | Cons (h, _) -> h | Nil -> 0"
      Ast.TInt;
    check
      "two mutually-referencing user-defined types type-check end to end"
      "type even_list = ENil | ECons of int * odd_list;\n\
       type odd_list = OCons of int * even_list;\n\
       ECons (1, OCons (2, (ENil)))"
      (Ast.TConstr "even_list");
    check_raises_type_error "an unbound variable is a type error" "z";
    check_raises_type_error "applying an int as a function is a type error" "5 6";
    check_raises_type_error "mismatched if branches is a type error"
      "if True then 1 else True";
  ]
