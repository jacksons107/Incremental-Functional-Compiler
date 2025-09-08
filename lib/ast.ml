type pat = 
    | PVar of string
    | PInt of int
    | PBool of bool
    | PCons of pat * pat
    | PEmpty
    | PConstr of string * pat list

type exp =
    | Var of string
    | Int of int
    | Bool of bool
    | Eq of exp * exp
    | IsEmpty of exp
    | IsCons of exp
    | IsInt of exp
    | IsConstr of exp * string
    | Plus of exp * exp
    | App of exp * exp
    | Let of string * exp * exp
    | Def of string * string list * exp * exp
    | Defrec of string * string list * exp * exp
    | Match of exp list * (pat list * exp) list
    | If of exp * exp * exp
    | Cons of exp * exp
    | Type of string * string * string list * exp
    | Pack of string * exp list
    | Unpack of exp * int
    | List of exp list
    | Head of exp
    | Tail of exp
    | Empty
    | Fail

type def =
    | DLet of string * exp
    | DDef of string * string list * exp
    | DDefrec of string * string list * exp

type prog = Prog of def list * exp