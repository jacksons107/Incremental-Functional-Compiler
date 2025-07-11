open Lam
open Comb

let rec abstract var exp = match exp with
    | CVar y when var = y -> I
    | CVar _ 
    | I | K | S           
    | CPlus               -> CApp (K, exp)
    | CInt n              -> CApp (K, CInt n)
    | CApp (e1, e2)       -> CApp (CApp (S, (abstract var e1)), (abstract var e2))

let rec lam_to_comb exp = match exp with
    | LVar x          -> CVar x
    | LInt n          -> CInt n
    | LPlus           -> CPlus
    | LApp (e1, e2)   -> CApp (lam_to_comb e1, lam_to_comb e2)
    | Lam (var, body) -> abstract var (lam_to_comb body)