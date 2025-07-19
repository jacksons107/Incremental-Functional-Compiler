open Ast_to_elam
open Elam_to_lam
open Lam_to_comb
open Comb_to_j
open J_machine

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

let compile exp =
    run_j_machine (comb_to_j (lam_to_comb (elam_to_lam (ast_to_elam (parse exp)))))


let exp = "2 + 3 + (-4)"
let entry = compile exp

let () =
  let oc = open_out "generated.c" in
  output_string oc "#include \"runtime.h\"\n";
  output_string oc "void entry() {\n";
  output_string oc (entry ^ "\n");
  output_string oc "}";
  close_out oc;
