open Lam
open Lam_to_comb
open Comb_to_j
open J_machine

let lam_expr = LInt 42
let comb_expr = lam_to_comb lam_expr
let j_code = comb_to_j comb_expr
let asm = run_j_machine j_code
let () =
  let oc = open_out "entry.s" in
  output_string oc "\t.globl _entry\n";
  output_string oc "_entry:\n";
  output_string oc asm;
  output_char oc '\n';
  close_out oc;
