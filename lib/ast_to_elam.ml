open Ast
open Elam

let rec ast_to_elam ast = match ast with
    | Int n         -> EInt n
    | Plus (e1, e2) -> EApp (EApp (EPlus, ast_to_elam e1), ast_to_elam e2)
