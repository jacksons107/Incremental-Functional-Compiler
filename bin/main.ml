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
  run_j_machine
    (comb_to_j (lam_to_comb (elam_to_lam (ast_to_elam (parse exp)))))

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
  let entry = compile program in

  (* Write generated.c *)
  let oc = open_out "generated.c" in
  output_string oc "#include \"runtime.h\"\n";
  output_string oc "void entry() {\n";
  output_string oc (entry ^ "\n");
  output_string oc "}";
  close_out oc;

  (* Call gcc to produce executable *)
  let cmd = "gcc -o prog generated.c runtime.c" in
  match Sys.command cmd with
  | 0 -> Printf.printf "Build successful. Run ./prog\n"
  | n -> Printf.eprintf "gcc failed with code %d\n" n
