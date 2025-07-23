type elam_exp = 
    | EVar of string
    | EInt of int
    | EBool of bool
    | EPlus
    | EIf
    | EApp of elam_exp * elam_exp
    | ELam of string * elam_exp