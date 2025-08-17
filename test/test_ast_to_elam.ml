open Ast
open Elam
open Ast_to_elam


let rec pp_elam elam_exp = match elam_exp with
    | EVar x -> "Var " ^ x
    | EInt n -> string_of_int n
    | EBool b -> string_of_bool b
    | EEq -> "=="
    | EPlus -> "+"
    | EIf -> "If"
    | EY -> "Y"
    | EHead -> "HEAD"
    | ETail -> "TAIL"
    | ECons -> "CONS"
    | EEmpty -> "[]"
    | EApp (e1, e2) -> "(" ^ pp_elam e1 ^ " " ^ pp_elam e2 ^ ")"
    | ELam (v, b) -> "(lambda (" ^ v ^ ") (" ^ pp_elam b ^ "))"
    | ELet (v, b, e) -> "(let " ^ v ^ " = " ^ pp_elam b ^ " in " ^ pp_elam e ^ ")"


let exp = Defrec ("sum", ["l"; "h"], If (Eq (Var "l", Var "h"), Var "l", Plus (Var "l", App (App (Var "sum", Plus (Var "l",Int 1)), Var "h"))), App (App (Var "sum", Int 0), Int 5))

let () = print_endline (pp_elam (ast_to_elam exp))