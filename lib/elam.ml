type elam_exp = 
    | EInt of int
    | EPlus
    | EApp of elam_exp * elam_exp
