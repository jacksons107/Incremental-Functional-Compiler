type exp =
    | Var of string
    | Int of int
    | Bool of bool
    | Plus of exp * exp
    | App of exp * exp
    | Let of string * exp * exp
    | Def of string * string list * exp * exp
    | If of exp * exp * exp