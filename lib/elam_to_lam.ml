open Elam
open Lam

let rec elam_to_lam expr = match expr with
    | EInt n -> LInt n 
    | EPlus  -> LPlus 
    | EApp (e1, e2) -> LApp (elam_to_lam e1, elam_to_lam e2)
