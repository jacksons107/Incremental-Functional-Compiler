type lam_exp = 
    | LVar of string
    | LInt of int
    | LBool of bool
    | LEq
    | LPlus
    | LIf
    | LHead
    | LTail
    | LCons
    | LEmpty
    | LConstr of string * int
    | LIsEmpty
    | LIsCons
    | LIsInt
    | LFail
    | LY
    | LApp of lam_exp * lam_exp
    | Lam of string * lam_exp