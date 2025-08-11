type comb_exp =
    | I | K | S
    | CInt of int
    | CBool of bool
    | CPlus
    | CIf
    | CY
    | CVar of string
    | CApp of comb_exp * comb_exp