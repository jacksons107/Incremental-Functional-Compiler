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
    | IsCons of exp
    | IsConstr of exp * string
    | Plus of exp * exp
    | App of exp * exp
    | Let of string * exp * exp
    | Def of string * string list * exp * exp
    | Defrec of string * string list * exp * exp
    | Match of exp list * (pat list * exp) list
    | If of exp * exp * exp
    | Cons of exp * exp
    (* | Type of string * string * string list * exp *)
    | Constr of string * string list * exp
    | Pack of string * exp list
    | Unpack of string * exp * int
    | List of exp list
    | Head of exp
    | Tail of exp
    | Empty
    | Fail

type def =
    | DLet of string * exp
    | DDef of string * string list * exp
    | DDefrec of string * string list * exp
    | DConstr of string * string * string list
    | DType of string * (string * string list) list

type typedef = 
    TypeDef of string * string * string list

type prog = Prog of def list * exp