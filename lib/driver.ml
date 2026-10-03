open Ast
open Desugar
open Ast_to_elam
open Type_infer
open Elam_to_lam
open Lam_to_comb
open Comb_to_j
open J_machine

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.fprintf outx "File \"%s\", line %d, column %d" pos.Lexing.pos_fname
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let parse filename s =
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try Parser.prog Lexer.read lexbuf
  with Parser.Error ->
    Printf.eprintf "%a: syntax error\n" print_position lexbuf;
    exit 1

let compile_to_c ~filename ~source =
  let (Prog (defs, exp)) = parse filename source in
  let ast_exp = def_to_exp (Prog (defs, exp)) in
  let typedefs = get_types defs in
  let env = setup_env typedefs empty_env in
  let elam = ast_to_elam ast_exp in
  let _ = infer elam env in
  run_j_machine (comb_to_j (lam_to_comb (elam_to_lam elam)))

let typecheck ~filename ~source =
  let (Prog (defs, exp)) = parse filename source in
  let ast_exp = def_to_exp (Prog (defs, exp)) in
  let typedefs = get_types defs in
  let env = setup_env typedefs empty_env in
  let elam = ast_to_elam ast_exp in
  infer elam env

let emit_c_file ~out_path ~entry_body =
  let oc = open_out out_path in
  output_string oc "#include \"runtime.h\"\n";
  output_string oc "void entry() {\n";
  output_string oc (entry_body ^ "\n");
  output_string oc "}";
  close_out oc

let build_exe ~c_file ~runtime_dir ~out_exe =
  let runtime_c = Filename.concat runtime_dir "runtime.c" in
  let utils_c = Filename.concat runtime_dir "utils.c" in
  let cmd =
    Printf.sprintf "gcc -o %s %s %s %s -I%s" (Filename.quote out_exe)
      (Filename.quote c_file) (Filename.quote runtime_c)
      (Filename.quote utils_c)
      (Filename.quote runtime_dir)
  in
  match Sys.command cmd with
  | 0 -> Ok ()
  | n -> Error (Printf.sprintf "gcc failed with code %d" n)
