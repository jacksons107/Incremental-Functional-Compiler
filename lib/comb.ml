type comb_exp =
    | I | K | S
    | CInt of int
    | CPlus
    | CVar of string
    | CApp of comb_exp * comb_exp