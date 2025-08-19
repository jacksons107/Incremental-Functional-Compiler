open Ast
open Elam

let rec curry vars body = match vars with
    | []      -> body
    | (x::xs) -> ELam (x, (curry xs body))

let rec pat_to_exp pat = match pat with
    | PVar x -> Var x
    | PInt n -> Int n
    | PBool b -> Bool b
    | PCons (x, xs) -> Cons (pat_to_exp x, pat_to_exp xs) 
    | PEmpty -> Empty

let rec list_to_cons list = match list with
    | [] -> Empty
    | (x::xs) -> Cons (x, list_to_cons xs)

let rec match_prod scrut pat expr = match pat with
    (* | PCons (x, y)              -> Let (x, Head scrut, Let (y, Tail scrut, expr)) *)
    | PCons (x, y)              -> match_prod (Head scrut) x (match_prod (Tail scrut) y expr)
    | PEmpty | PInt _ | PBool _ -> expr
    | PVar x                    -> Let (x, scrut, expr)

let rec match_sum scrut cases = match cases with
    | [] -> Fail
    | (p, e)::cs -> match p with
        | PEmpty  -> If (IsEmpty scrut, match_prod scrut p e, match_sum scrut cs)
        | PCons _ -> If (IsCons scrut, match_prod scrut p e, match_sum scrut cs)
        | _       -> failwith "Skipping these for now"


let rec ast_to_elam ast = match ast with
    | Var x                           -> EVar x
    | Int n                           -> EInt n
    | Bool b                          -> EBool b
    | Empty                           -> EEmpty
    | App (e1, e2)                    -> EApp (ast_to_elam e1, ast_to_elam e2)
    | Eq (e1, e2)                     -> EApp (EApp (EEq, ast_to_elam e1), ast_to_elam e2)
    | IsEmpty e                       -> EApp (EIsEmpty, ast_to_elam e)
    | IsCons e                        -> EApp (EIsCons, ast_to_elam e)
    | Plus (e1, e2)                   -> EApp (EApp (EPlus, ast_to_elam e1), ast_to_elam e2)
    | If (b, e1, e2)                  -> EApp (EApp (EApp (EIf, ast_to_elam b), ast_to_elam e1), ast_to_elam e2)
    | Head c                          -> EApp (EHead, ast_to_elam c)
    | Tail c                          -> EApp (ETail, ast_to_elam c)
    | Cons (e1, e2)                   -> EApp (EApp (ECons, ast_to_elam e1), ast_to_elam e2)
    | List d                          -> ast_to_elam (list_to_cons d)
    | Let (var, b, e)                 -> ELet (var, ast_to_elam b, ast_to_elam e)
    | Def (name, vars, body, rest)    -> ELet (name, curry vars (ast_to_elam body), ast_to_elam rest)
    | Defrec (name, vars, body, rest) -> ELet (name, EApp (EY, curry ([name] @ vars) (ast_to_elam body)), ast_to_elam rest)
    | Fail                            -> EFail
    | Match (scrut, cases)            -> ast_to_elam (match_sum scrut cases)


