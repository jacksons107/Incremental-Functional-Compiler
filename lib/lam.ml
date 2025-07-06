type lam_exp = 
    | LVar of string
    | LInt of int
    | LPlus
    | LApp of lam_exp * lam_exp
    | Lam of string * lam_exp