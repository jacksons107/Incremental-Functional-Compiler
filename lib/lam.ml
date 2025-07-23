type lam_exp = 
    | LVar of string
    | LInt of int
    | LBool of bool
    | LPlus
    | LIf
    | LApp of lam_exp * lam_exp
    | Lam of string * lam_exp