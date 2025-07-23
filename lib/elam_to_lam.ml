open Elam
open Lam

let rec elam_to_lam expr = match expr with
    | EVar x           -> LVar x
    | EInt n           -> LInt n 
    | EBool b          -> LBool b
    | EPlus            -> LPlus 
    | EIf              -> LIf
    | EApp (e1, e2)    -> LApp (elam_to_lam e1, elam_to_lam e2)
    | ELam (var, body) -> Lam (var, elam_to_lam body)