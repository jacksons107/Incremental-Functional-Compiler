open Ast
open Elam

let rec curry vars body = match vars with
    | []      -> body
    | (x::xs) -> ELam (x, (curry xs body))

let rec ast_to_elam ast = match ast with
    | Var x                       -> EVar x
    | Int n                       -> EInt n
    | Bool b                      -> EBool b
    | App (e1, e2)                -> EApp (ast_to_elam e1, ast_to_elam e2)
    | Plus (e1, e2)               -> EApp (EApp (EPlus, ast_to_elam e1), ast_to_elam e2)
    | If (b, e1, e2)              -> EApp (EApp (EApp (EIf, ast_to_elam b), ast_to_elam e1), ast_to_elam e2)
    | Head c                      -> EApp (EHead, ast_to_elam c)
    | Tail c                      -> EApp (ETail, ast_to_elam c)
    | Cons (e1, e2)               -> EApp (EApp (ECons, ast_to_elam e1), ast_to_elam e2)
    | Let (var, b, e)             -> ELet (var, ast_to_elam b, ast_to_elam e)
    | Def (name, var, body, rest) -> ELet (name, curry var (ast_to_elam body), ast_to_elam rest)

