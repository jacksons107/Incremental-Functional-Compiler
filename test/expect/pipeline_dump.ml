open Compiler
open Ast

(* Dumps the pretty-printed IR at each successive pipeline stage: the
   desugared surface Ast (pattern matches not yet compiled -- that happens
   transparently inside Ast_to_elam), Elam, Lam, and Comb. Useful both as a
   snapshot regression test and as a way to actually see how each lowering
   pass transforms a program. *)
let show src =
  let (Prog (defs, e)) = Parser.prog Lexer.read (Lexing.from_string src) in
  let ast = Desugar.def_to_exp (Prog (defs, e)) in
  Format.printf "Ast:  %a@." Ast.pp_exp ast;
  let elam = Ast_to_elam.ast_to_elam ast in
  Format.printf "Elam: %a@." Elam.pp elam;
  let lam = Elam_to_lam.elam_to_lam elam in
  Format.printf "Lam:  %a@." Lam.pp lam;
  let comb = Lam_to_comb.lam_to_comb lam in
  Format.printf "Comb: %a@." Comb.pp comb

let%expect_test "arithmetic + let" =
  show "let x = 5;\nx + 1";
  [%expect
    {|
    Ast:  let x = 5 in (x + 1)
    Elam: let x = 5 in ((+ x) 1)
    Lam:  ((\x -> ((+ x) 1)) 5)
    Comb: (((S ((S (K +)) I)) (K 1)) 5)
    |}]

let%expect_test "recursion via defrec (Y combinator)" =
  show "defrec fact n = if n == 0 then 1 else n + fact (n + -1);\nfact 5";
  [%expect
    {|
    Ast:  defrec fact n = if (n == 0) then 1 else (n + (fact (n + -1))) in (fact 5)
    Elam: let fact = (Y (\fact -> (\n -> (((IF ((== n) 0)) 1) ((+ n) (fact ((+ n) -1))))))) in (fact 5)
    Lam:  ((\fact -> (fact 5)) (Y (\fact -> (\n -> (((IF ((== n) 0)) 1) ((+ n) (fact ((+ n) -1))))))))
    Comb: (((S I) (K 5)) (Y ((S ((S (K S)) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K IF)))) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K ==)))) (K I)))) ((S (K K)) (K 0)))))) ((S (K K)) (K 1))))) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K +)))) (K I)))) ((S ((S (K S)) ((S (K K)) I))) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K +)))) (K I)))) ((S (K K)) (K -1))))))))
    |}]

let%expect_test "list literal + cons-pattern match" =
  show "match [1,2] with | h::t -> h";
  [%expect
    {|
    Ast:  match [[1, 2]] with (h :: t) -> h
    Elam: (((IF (IsCons ((CONS 1) ((CONS 2) [])))) let h = (HEAD ((CONS 1) ((CONS 2) []))) in let t = (TAIL ((CONS 1) ((CONS 2) []))) in h) Fail)
    Lam:  (((IF (IsCons ((CONS 1) ((CONS 2) [])))) ((\h -> ((\t -> h) (TAIL ((CONS 1) ((CONS 2) []))))) (HEAD ((CONS 1) ((CONS 2) []))))) Fail)
    Comb: (((IF (IsCons ((CONS 1) ((CONS 2) [])))) (((S ((S (K K)) I)) ((S (K TAIL)) ((S ((S (K CONS)) (K 1))) ((S ((S (K CONS)) (K 2))) (K []))))) (HEAD ((CONS 1) ((CONS 2) []))))) Fail)
    |}]

let%expect_test "sum type + constructor-pattern match" =
  show
    "type pair = Pair of int * int;\nmatch Pair(1,2) with | Pair(x, y) -> x + y";
  [%expect
    {|
    Ast:  Constr(Pair, [int, int], match [Pair(1, 2)] with Pair(x, y) -> (x + y))
    Elam: let Pair = Constr(Pair, 2) in (((IF ((IsConstr ((Pair 1) 2)) "Pair")) let x = Unpack(Pair, ((Pair 1) 2), 0) in let y = Unpack(Pair, ((Pair 1) 2), 1) in ((+ x) y)) Fail)
    Lam:  ((\Pair -> (((IF ((IsConstr ((Pair 1) 2)) "Pair")) ((\x -> ((\y -> ((+ x) y)) ((Unpack ((Pair 1) 2)) 1))) ((Unpack ((Pair 1) 2)) 0))) Fail)) Constr(Pair, 2))
    Comb: (((S ((S ((S (K IF)) ((S ((S (K IsConstr)) ((S ((S I) (K 1))) (K 2)))) (K "Pair")))) ((S ((S ((S (K S)) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K S)))) ((S ((S (K S)) ((S (K K)) (K K)))) ((S (K K)) (K +)))))) ((S ((S (K S)) ((S (K K)) (K K)))) (K I)))))) ((S (K K)) (K I))))) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) (K Unpack)))) ((S ((S (K S)) ((S ((S (K S)) ((S (K K)) I))) ((S (K K)) (K 1))))) ((S (K K)) (K 2)))))) ((S (K K)) (K 1))))) ((S ((S (K Unpack)) ((S ((S I) (K 1))) (K 2)))) (K 0))))) (K Fail)) Constr(Pair, 2))
    |}]
