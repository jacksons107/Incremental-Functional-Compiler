open Ast
open Elam
open Patterns

let rec curry vars body = match vars with
    | []      -> body
    | (x::xs) -> ELam (x, (curry xs body))

let rec list_to_cons list = match list with
    | []      -> Empty
    | (x::xs) -> Cons (x, list_to_cons xs)

let rec ast_to_elam ast = match ast with
    | Var x                           -> EVar x
    | Int n                           -> EInt n
    | Bool b                          -> EBool b
    | Empty                           -> EEmpty
    | App (e1, e2)                    -> EApp (ast_to_elam e1, ast_to_elam e2)
    | Eq (e1, e2)                     -> EApp (EApp (EEq, ast_to_elam e1), ast_to_elam e2)
    | IsCons e                        -> EApp (EIsCons, ast_to_elam e)
    | IsConstr (e, name)              -> EApp (EApp (EIsConstr, ast_to_elam e), EConstr (name, -1))
    | Plus (e1, e2)                   -> EApp (EApp (EPlus, ast_to_elam e1), ast_to_elam e2)
    | If (b, e1, e2)                  -> EApp (EApp (EApp (EIf, ast_to_elam b), ast_to_elam e1), ast_to_elam e2)
    | Head c                          -> EApp (EHead, ast_to_elam c)
    | Tail c                          -> EApp (ETail, ast_to_elam c)
    | Cons (e1, e2)                   -> EApp (EApp (ECons, ast_to_elam e1), ast_to_elam e2)
    | Type (_, name, args, rest)      -> ELet (name, EConstr (name, List.length args), ast_to_elam rest)
    | Pack (name, args)               -> app_constr (EVar name) (List.rev args)
    | Unpack (cname, struc, idx)      -> EUnpack (cname, ast_to_elam struc, idx)
    | List d                          -> ast_to_elam (list_to_cons d)
    | Let (var, b, e)                 -> ELet (var, ast_to_elam b, ast_to_elam e)
    | Def (name, vars, body, rest)    -> ELet (name, curry vars (ast_to_elam body), ast_to_elam rest)
    | Defrec (name, vars, body, rest) -> ELet (name, EApp (EY, curry ([name] @ vars) (ast_to_elam body)), ast_to_elam rest)
    | Fail                            -> EFail
    | Match (scrut, cases)            -> ast_to_elam (compile_match (Scruts scrut) (Matrix cases))
and app_constr constr args = match args with
    | []      -> constr
    | (x::xs) -> EApp (app_constr constr xs, ast_to_elam x)