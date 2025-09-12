open Elam
open Ast_to_elam
open Def_to_exp

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

let rec pp_elam elam_exp = match elam_exp with
    | EVar x -> "Var " ^ x
    | EInt n -> string_of_int n
    | EBool b -> string_of_bool b
    | EString s -> s
    | EEq -> "=="
    | EIsCons -> "IsCons"
    | EPlus -> "+"
    | EUnpack (n, s, i) -> "Unpack(" ^ n ^ ", " ^ pp_elam s ^ ", " ^ string_of_int i ^ ")"
    | EIsConstr -> "IsConstr"
    | EIf -> "If"
    | EY -> "Y"
    | EHead -> "HEAD"
    | ETail -> "TAIL"
    | ECons -> "CONS"
    | EConstr (c, n) -> "Constr (" ^ c ^ ", " ^ string_of_int n ^ ")"
    | EEmpty -> "[]"
    | EFail -> "Fail"
    | EApp (e1, e2) -> "(" ^ pp_elam e1 ^ " " ^ pp_elam e2 ^ ")"
    | ELam (v, b) -> "(lambda (" ^ v ^ ") (" ^ pp_elam b ^ "))"
    | ELet (v, b, e) -> "(let " ^ v ^ " = " ^ pp_elam b ^ " in " ^ pp_elam e ^ ")"

let exp = "type pair = Pair of int * int | None;
            let test = Pair (69, 420);
            match test with 
                | None -> 0
                | Pair (69, 420) -> 420"

let () = print_endline (pp_elam (ast_to_elam (def_to_exp (parse exp))))