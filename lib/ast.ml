type exp =
    | Var of string
    | Int of int
    | Bool of bool
    | Plus of exp * exp
    | Let of string * exp * exp
    | If of exp * exp * exp