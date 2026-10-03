open Compiler
open Ast

(* Elaborates one top-level def's own body directly via Ast_to_elam.def_to_elam,
   then lowers it with the ordinary Elam_to_lam.elam_to_lam, bypassing
   Desugar.def_to_exp's whole-program chaining entirely -- there's no enclosing
   Let and no trailing continuation. *)
let show src =
  let (Prog (defs, _)) = Parser.prog Lexer.read (Lexing.from_string src) in
  match defs with
  | [ d ] ->
      let name, elam = Ast_to_elam.def_to_elam d in
      let lam = Elam_to_lam.elam_to_lam elam in
      Format.printf "name: %s@." name;
      Format.printf "Elam: %a@." Elam.pp elam;
      Format.printf "Lam:  %a@." Lam.pp lam
  | _ -> failwith "show: expected exactly one def"

let show_raises src =
  let (Prog (defs, _)) = Parser.prog Lexer.read (Lexing.from_string src) in
  match defs with
  | [ d ] -> (
      try ignore (Ast_to_elam.def_to_elam d)
      with Failure msg -> Format.printf "Raised: %s@." msg)
  | _ -> failwith "show_raises: expected exactly one def"

let%expect_test "DLet" =
  show "let x = 5;";
  [%expect {|
    name: x
    Elam: 5
    Lam:  5
    |}]

let%expect_test "DDef" =
  show "def id x = x;";
  [%expect {|
    name: id
    Elam: (\x -> x)
    Lam:  (\x -> x)
    |}]

let%expect_test "DDefrec" =
  show "defrec fact n = if n == 0 then 1 else n + fact (n + -1);";
  [%expect
    {|
    name: fact
    Elam: (Y (\fact -> (\n -> (((IF ((== n) 0)) 1) ((+ n) (fact ((+ n) -1)))))))
    Lam:  (Y (\fact -> (\n -> (((IF ((== n) 0)) 1) ((+ n) (fact ((+ n) -1)))))))
    |}]

let%expect_test "DType has no body to elaborate" =
  show_raises "type pair = Pair of int * int;";
  [%expect
    {| Raised: def_to_elam: type definitions have no body to elaborate |}]
