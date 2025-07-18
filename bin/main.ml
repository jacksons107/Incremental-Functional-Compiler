open Lam
open Lam_to_comb
open Comb_to_j
open J_machine

let lam_expr = LApp ((LApp (LPlus, LInt 2)), LInt 3) 
let comb_expr = lam_to_comb lam_expr
let j_code = comb_to_j comb_expr
let entry = run_j_machine j_code
let () =
  let oc = open_out "generated.c" in
  output_string oc "#include \"runtime.h\"\n";
  output_string oc "void entry() {\n";
  output_string oc (entry ^ "\n");
  output_string oc "}";
  close_out oc;
