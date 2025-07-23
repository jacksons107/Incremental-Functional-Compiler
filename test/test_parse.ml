open Ast

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast



let rec pp_ast exp = match exp with
  | Var x   -> x
  | Int n   -> string_of_int n
  | Bool b  -> string_of_bool b
  | Plus (e1, e2) -> "Plus (" ^ pp_ast e1 ^ ", " ^ pp_ast e2 ^ ")"
  | If (b, e1, e2) -> "If (" ^ pp_ast b ^ ", " ^ pp_ast e1 ^ ", " ^ pp_ast e2 ^ ")"
  | Let (v, b, e) -> "Let (" ^ v ^ ", " ^ pp_ast b ^ ", " ^ pp_ast e ^ ")"

let exp = "let x = 3 in (let y = 4 in x + y)"

let () = print_endline (pp_ast (parse exp))