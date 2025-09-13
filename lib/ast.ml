type typ = 
    | TInt
    | TBool
    | TString
    | TLam of typ * typ
    | TList of typ
    | TConstr of string
    | TVar of tyvar ref

and tyvar = 
    | Unbound of int
    | Link of typ

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
    | Constr of string * typ list * exp
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
    | DType of string * (string * typ list) list

type typedef = 
    TypeDef of string * string * typ list

type prog = Prog of def list * exp