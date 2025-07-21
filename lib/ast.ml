type exp =
    | Var of string
    | Int of int
    | Plus of exp * exp
    | Let of string * exp * exp