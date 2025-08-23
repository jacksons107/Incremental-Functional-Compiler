open Ast
open Elam

let rec curry vars body = match vars with
    | []      -> body
    | (x::xs) -> ELam (x, (curry xs body))

let rec list_to_cons list = match list with
    | []      -> Empty
    | (x::xs) -> Cons (x, list_to_cons xs)

let rec match_sum scrut cases =
    match cases with
    | [] -> Fail
    | (p, e) :: cs ->
        match_prod scrut p e (match_sum scrut cs)

and match_prod scrut pat expr on_fail =
    match pat with
    | PVar x ->
        Let (x, scrut, expr)

    | PInt n ->
        If (Eq (scrut, Int n), expr, on_fail)

    | PBool b ->
        If (Eq (scrut, Bool b), expr, on_fail)

    | PEmpty ->
        If (IsEmpty scrut, expr, on_fail)

    | PCons (p1, p2) ->
        If (IsCons scrut,
                match_prod (Head scrut) p1
                (match_prod (Tail scrut) p2 expr on_fail)
                on_fail,
            on_fail)


let rec ast_to_elam ast = match ast with
    | Var x                           -> EVar x
    | Int n                           -> EInt n
    | Bool b                          -> EBool b
    | Empty                           -> EEmpty
    | App (e1, e2)                    -> EApp (ast_to_elam e1, ast_to_elam e2)
    | Eq (e1, e2)                     -> EApp (EApp (EEq, ast_to_elam e1), ast_to_elam e2)
    | IsEmpty e                       -> EApp (EIsEmpty, ast_to_elam e)
    | IsCons e                        -> EApp (EIsCons, ast_to_elam e)
    | IsInt e                         -> EApp (EIsInt, ast_to_elam e)
    | Plus (e1, e2)                   -> EApp (EApp (EPlus, ast_to_elam e1), ast_to_elam e2)
    | If (b, e1, e2)                  -> EApp (EApp (EApp (EIf, ast_to_elam b), ast_to_elam e1), ast_to_elam e2)
    | Head c                          -> EApp (EHead, ast_to_elam c)
    | Tail c                          -> EApp (ETail, ast_to_elam c)
    | Cons (e1, e2)                   -> EApp (EApp (ECons, ast_to_elam e1), ast_to_elam e2)
    | Type (_, name, args, rest)      -> ELet (name, EConstr (name, List.length args), ast_to_elam rest)
    | List d                          -> ast_to_elam (list_to_cons d)
    | Let (var, b, e)                 -> ELet (var, ast_to_elam b, ast_to_elam e)
    | Def (name, vars, body, rest)    -> ELet (name, curry vars (ast_to_elam body), ast_to_elam rest)
    | Defrec (name, vars, body, rest) -> ELet (name, EApp (EY, curry ([name] @ vars) (ast_to_elam body)), ast_to_elam rest)
    | Fail                            -> EFail
    | Match (scrut, cases)            -> ast_to_elam (match_sum scrut cases)
