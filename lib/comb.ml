type comb_exp =
    | I | K | S
    | CInt of int
    | CBool of bool
    | CPlus
    | CIf
    | CHead
    | CTail
    | CCons of comb_exp * comb_exp
    | CY
    | CVar of string
    | CApp of comb_exp * comb_exp