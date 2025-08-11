open Ast_to_elam
open Elam_to_lam
open Lam_to_comb
open Comb_to_j
open J_machine
(* open Comb *)
(* open Lam *)

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

let compile exp =
    run_j_machine (comb_to_j (lam_to_comb (elam_to_lam (ast_to_elam (parse exp)))))


(* let exp = "def add4 x = x + 4 in add4 3" *)
(* let exp = "def not b = if b then False else True in not True" *)
let exp = "def add x y = x + y in add 69 420"

(* let exp = "5 + (5 + 6)" *)
(* let exp = "let x = 3 in x + x + 5" *)
(* let exp = "5 + (let x = 3 in x + 4)" *)
(* let exp = "let x = 3 in let y = 4 in x + y" *)
(* let exp = "let x = False in if x then 1 else 2" *)
let entry = compile exp

(* let exp = CApp ((CApp (K ,(CApp (I, (CApp ((CApp (CPlus, CInt 4)), CInt 5)))))), CInt 6) *)

(* S K S 3 = K 3 (S 3) = 3 *)
(* let exp = CApp ((CApp ((CApp (S, K)), S)), CInt 3) *)

(* S + I 3 = + 3 (I 3) *)
(* let exp = CApp ((CApp ((CApp (S, CPlus)), I)), CInt 3) *)

(* (((S ((S (K +)) I)) I) 5)  *)
(* let exp = CApp ((CApp ((CApp (S, (CApp ((CApp (S, (CApp (K, CPlus)))), I)))), I)), CInt 5) *)

(* let double = (LApp ((LApp (LPlus, (LVar "x"))), (LVar "x")))
let lam_double = (Lam ("x", double))
let exp = LApp (lam_double, LInt 3)

let entry = run_j_machine (comb_to_j (lam_to_comb exp)) *)

let () =
  let oc = open_out "generated.c" in
  output_string oc "#include \"runtime.h\"\n";
  output_string oc "void entry() {\n";
  output_string oc (entry ^ "\n");
  output_string oc "}";
  close_out oc;
