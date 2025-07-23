open Ast
open Elam

let rec ast_to_elam ast = match ast with
    | Var x           -> EVar x
    | Int n           -> EInt n
    | Bool b          -> EBool b
    | Plus (e1, e2)   -> EApp (EApp (EPlus, ast_to_elam e1), ast_to_elam e2)
    | If (b, e1, e2)  -> EApp (EApp (EApp (EIf, ast_to_elam b), ast_to_elam e1), ast_to_elam e2)
    | Let (var, b, e) -> EApp (ELam (var, ast_to_elam e), ast_to_elam b)
