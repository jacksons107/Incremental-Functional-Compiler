open Ast
open Lam

let rec ast_to_lam ast = match ast with
    | Int n -> LInt n
    | Plus (e1, e2) -> LApp (LApp (LPlus, ast_to_lam e1), ast_to_lam e2)
