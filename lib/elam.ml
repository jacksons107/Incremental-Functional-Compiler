type elam_exp = 
    | EVar of string
    | EInt of int
    | EBool of bool
    | EPlus
    | EIf
    | EHead
    | ETail
    | ECons
    | EEmpty
    | EApp of elam_exp * elam_exp
    | ELam of string * elam_exp
    | ELet of string * elam_exp * elam_exp