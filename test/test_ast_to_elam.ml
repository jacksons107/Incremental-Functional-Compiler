open Elam
open Ast_to_elam

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

let rec pp_elam elam_exp = match elam_exp with
    | EVar x -> "Var " ^ x
    | EInt n -> string_of_int n
    | EBool b -> string_of_bool b
    | EEq -> "=="
    | EIsEmpty -> "IsEmpty"
    | EIsCons -> "IsCons"
    | EPlus -> "+"
    | EIf -> "If"
    | EY -> "Y"
    | EHead -> "HEAD"
    | ETail -> "TAIL"
    | ECons -> "CONS"
    | EEmpty -> "[]"
    | EFail -> "Fail"
    | EApp (e1, e2) -> "(" ^ pp_elam e1 ^ " " ^ pp_elam e2 ^ ")"
    | ELam (v, b) -> "(lambda (" ^ v ^ ") (" ^ pp_elam b ^ "))"
    | ELet (v, b, e) -> "(let " ^ v ^ " = " ^ pp_elam b ^ " in " ^ pp_elam e ^ ")"

let exp = "[1, 2, 3]"

let () = print_endline (pp_elam (ast_to_elam (parse exp)))