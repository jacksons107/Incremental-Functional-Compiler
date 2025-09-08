open Def_to_exp
open Ast_to_elam
open Elam_to_lam
open Lam_to_comb
open Comb_to_j
open J_machine

(* TODO -- handle module interfaces in a cleaner way *)

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.fprintf outx "File \"%s\", line %d, column %d"
    pos.Lexing.pos_fname
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let parse filename s =
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    Parser.prog Lexer.read lexbuf
  with
  (* | Lexer.Error msg ->
      Printf.eprintf "%a: lexer error: %s\n" print_position lexbuf msg;
      exit 1 *)
  | Parser.Error ->
      Printf.eprintf "%a: syntax error\n" print_position lexbuf;
      exit 1

let compile filename exp =
  run_j_machine
    (comb_to_j (lam_to_comb (elam_to_lam (ast_to_elam (def_to_exp (parse filename exp))))))

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "Usage: build <file.oj>";
    exit 1
  );

  let filename = Sys.argv.(1) in
  if not (Filename.check_suffix filename ".oj") then (
    prerr_endline "Error: input file must have .oj extension";
    exit 1
  );

  (* Read program from file *)
  let program =
    let ch = open_in filename in
    let len = in_channel_length ch in
    let s = really_input_string ch len in
    close_in ch;
    s
  in

  (* Compile to C code string *)
  let entry = compile filename program in

  (* Write generated.c *)
  let oc = open_out "generated.c" in
  output_string oc "#include \"runtime.h\"\n";
  output_string oc "void entry() {\n";
  output_string oc (entry ^ "\n");
  output_string oc "}";
  close_out oc;

  (* Call gcc to produce executable *)
  let cmd = "gcc -o prog generated.c runtime.c utils.c" in
  match Sys.command cmd with
  | 0 -> Printf.printf "Build successful. Run ./prog\n"
  | n -> Printf.eprintf "gcc failed with code %d\n" n
