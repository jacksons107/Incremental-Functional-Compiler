open Ast

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast



let rec pp_ast exp = match exp with
    | Var x   -> x
    | Int n   -> string_of_int n
    | Bool b  -> string_of_bool b
    | App (e1, e2) -> "App (" ^ pp_ast e1 ^ ", " ^ pp_ast e2 ^ ")"
    | Plus (e1, e2) -> "Plus (" ^ pp_ast e1 ^ ", " ^ pp_ast e2 ^ ")"
    | If (b, e1, e2) -> "If (" ^ pp_ast b ^ ", " ^ pp_ast e1 ^ ", " ^ pp_ast e2 ^ ")"
    | Let (v, b, e) -> "Let (" ^ v ^ ", " ^ pp_ast b ^ ", " ^ pp_ast e ^ ")"
    | Def (n, v, e, r) -> "Def (" ^ n ^ ", " ^ (String.concat " " v) ^ ", " ^ pp_ast e ^ ", " ^ pp_ast r ^ ")"


(* let exp = "let x = 3 in (let y = 4 in x + y)" *)
(* let exp = "def add4 x = x + 4 in add4 3" *)
(* let exp = "def not b = if b then False else True in not True" *)
let exp = "def add x y = x + y in add 69 420"

let () = print_endline (pp_ast (parse exp))