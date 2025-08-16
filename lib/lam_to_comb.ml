open Lam
open Comb

let rec abstract var exp = match exp with
    | CVar y when var = y -> I
    | CVar _ 
    | I | K | S
    | CIf
    | CHead | CTail
    | CY
    | CPlus               -> CApp (K, exp)
    | CCons (e1, e2)      -> CApp (K, CCons (abstract var e1, abstract var e2))
    | CInt n              -> CApp (K, CInt n)
    | CBool b             -> CApp (K, CBool b)
    | CApp (e1, e2)       -> CApp (CApp (S, (abstract var e1)), (abstract var e2))

let rec lam_to_comb exp = match exp with
    | LVar x          -> CVar x
    | LInt n          -> CInt n
    | LBool b         -> CBool b
    | LIf             -> CIf
    | LHead           -> CHead
    | LTail           -> CTail
    | LCons (e1, e2)  -> CCons (lam_to_comb e1, lam_to_comb e2)
    | LY              -> CY
    | LPlus           -> CPlus
    | LApp (e1, e2)   -> CApp (lam_to_comb e1, lam_to_comb e2)
    | Lam (var, body) -> abstract var (lam_to_comb body)