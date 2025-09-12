type comb_exp =
    | I | K | S
    | CInt of int
    | CBool of bool
    | CString of string
    | CEq
    | CPlus
    | CIf
    | CHead
    | CTail
    | CCons
    | CEmpty
    | CConstr of string * int
    | CUnpack
    | CIsCons
    | CIsConstr
    | CFail
    | CY
    | CVar of string
    | CApp of comb_exp * comb_exp