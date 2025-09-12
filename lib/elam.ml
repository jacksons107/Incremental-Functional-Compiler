type elam_exp = 
    | EVar of string
    | EInt of int
    | EBool of bool
    | EEq
    | EPlus
    | EIf
    | EY
    | EHead
    | ETail
    | ECons
    | EEmpty
    | EConstr of string * int
    | EUnpack of string * elam_exp * int
    | EIsCons
    | EIsConstr
    | EFail
    | EApp of elam_exp * elam_exp
    | ELam of string * elam_exp
    | ELet of string * elam_exp * elam_exp