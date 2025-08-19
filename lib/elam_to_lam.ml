open Elam
open Lam

let rec elam_to_lam expr = match expr with
    | EVar x           -> LVar x
    | EInt n           -> LInt n 
    | EBool b          -> LBool b
    | EEq              -> LEq
    | EPlus            -> LPlus 
    | EIf              -> LIf
    | EY               -> LY
    | EHead            -> LHead
    | ETail            -> LTail
    | ECons            -> LCons
    | EEmpty           -> LEmpty
    | EIsEmpty         -> LIsEmpty
    | EIsCons          -> LIsCons
    | EFail            -> LFail
    | EApp (e1, e2)    -> LApp (elam_to_lam e1, elam_to_lam e2)
    | ELam (var, body) -> Lam (var, elam_to_lam body)
    | ELet (var, b, e) -> LApp (Lam (var, elam_to_lam e), elam_to_lam b)