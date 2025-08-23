type pat = 
    | PVar of string
    | PInt of int
    | PBool of bool
    | PCons of pat * pat
    | PEmpty

type exp =
    | Var of string
    | Int of int
    | Bool of bool
    | Eq of exp * exp
    | IsEmpty of exp
    | IsCons of exp
    | IsInt of exp
    | Plus of exp * exp
    | App of exp * exp
    | Let of string * exp * exp
    | Def of string * string list * exp * exp
    | Defrec of string * string list * exp * exp
    | Match of exp * (pat * exp) list
    | If of exp * exp * exp
    | Cons of exp * exp
    | Type of string * string * string list * exp
    | Pack of string * exp list
    | List of exp list
    | Head of exp
    | Tail of exp
    | Empty
    | Fail