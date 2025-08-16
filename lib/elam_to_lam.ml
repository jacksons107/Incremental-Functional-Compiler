open Elam
open Lam

let rec elam_to_lam expr = match expr with
    | EVar x           -> LVar x
    | EInt n           -> LInt n 
    | EBool b          -> LBool b
    | EPlus            -> LPlus 
    | EIf              -> LIf
    | EHead            -> LHead
    | ETail            -> LTail
    | ECons (e1, e2)   -> LCons (elam_to_lam e1, elam_to_lam e2)
    | EApp (e1, e2)    -> LApp (elam_to_lam e1, elam_to_lam e2)
    | ELam (var, body) -> Lam (var, elam_to_lam body)
    | ELet (var, b, e) -> LApp (Lam (var, elam_to_lam e), elam_to_lam b)