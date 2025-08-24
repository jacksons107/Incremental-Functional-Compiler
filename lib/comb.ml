type comb_exp =
    | I | K | S
    | CInt of int
    | CBool of bool
    | CEq
    | CPlus
    | CIf
    | CHead
    | CTail
    | CCons
    | CEmpty
    | CConstr of string * int
    | CUnpack
    | CIsEmpty
    | CIsCons
    | CIsInt
    | CIsConstr
    | CFail
    | CY
    | CVar of string
    | CApp of comb_exp * comb_exp