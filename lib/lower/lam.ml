type lam_exp = 
    | LVar of string
    | LInt of int
    | LBool of bool
    | LString of string
    | LEq
    | LPlus
    | LIf
    | LHead
    | LTail
    | LCons
    | LEmpty
    | LConstr of string * int
    | LUnpack
    | LIsCons
    | LIsConstr
    | LFail
    | LY
    | LApp of lam_exp * lam_exp
    | Lam of string * lam_exp