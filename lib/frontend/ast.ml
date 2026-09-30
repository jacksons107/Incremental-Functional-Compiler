module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

type typ =
  | TInt
  | TBool
  | TString
  | TLam of typ * typ
  | TList of typ
  | TConstr of string
  | TVar of tyvar ref

and tyvar = Unbound of int | Link of typ

type tyscheme = Forall of IntSet.t * typ

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

type typedef = TypeDef of string * string * typ list
type prog = Prog of def list * exp

let rec pp_typ fmt t =
  match t with
  | TInt -> Format.fprintf fmt "int"
  | TBool -> Format.fprintf fmt "bool"
  | TString -> Format.fprintf fmt "string"
  | TLam (a, b) -> Format.fprintf fmt "(%a -> %a)" pp_typ a pp_typ b
  | TList t -> Format.fprintf fmt "%a list" pp_typ t
  | TConstr c -> Format.fprintf fmt "%s" c
  | TVar { contents = Unbound n } -> Format.fprintf fmt "'_%d" n
  | TVar { contents = Link t } -> pp_typ fmt t

let rec pp_pat fmt p =
  match p with
  | PVar v -> Format.fprintf fmt "%s" v
  | PInt n -> Format.fprintf fmt "%d" n
  | PBool b -> Format.fprintf fmt "%b" b
  | PCons (x, y) -> Format.fprintf fmt "(%a :: %a)" pp_pat x pp_pat y
  | PEmpty -> Format.fprintf fmt "[]"
  | PConstr (c, ps) ->
      Format.fprintf fmt "%s(%a)" c
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp_pat)
        ps

let pp_pats fmt ps =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
    pp_pat fmt ps

let rec pp_exp fmt e =
  match e with
  | Var x -> Format.fprintf fmt "%s" x
  | Int n -> Format.fprintf fmt "%d" n
  | Bool b -> Format.fprintf fmt "%b" b
  | Eq (a, b) -> Format.fprintf fmt "(%a == %a)" pp_exp a pp_exp b
  | IsCons e -> Format.fprintf fmt "IsCons(%a)" pp_exp e
  | IsConstr (e, c) -> Format.fprintf fmt "IsConstr(%a, %s)" pp_exp e c
  | Plus (a, b) -> Format.fprintf fmt "(%a + %a)" pp_exp a pp_exp b
  | App (f, a) -> Format.fprintf fmt "(%a %a)" pp_exp f pp_exp a
  | Let (v, b, e) -> Format.fprintf fmt "let %s = %a in %a" v pp_exp b pp_exp e
  | Def (f, vs, b, e) ->
      Format.fprintf fmt "def %s %s = %a in %a" f (String.concat " " vs) pp_exp
        b pp_exp e
  | Defrec (f, vs, b, e) ->
      Format.fprintf fmt "defrec %s %s = %a in %a" f (String.concat " " vs)
        pp_exp b pp_exp e
  | Match (scruts, cases) ->
      Format.fprintf fmt "match [%a] with %a"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp_exp)
        scruts
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " | ")
           (fun fmt (ps, rhs) ->
             Format.fprintf fmt "%a -> %a" pp_pats ps pp_exp rhs))
        cases
  | If (c, t, e) ->
      Format.fprintf fmt "if %a then %a else %a" pp_exp c pp_exp t pp_exp e
  | Cons (a, b) -> Format.fprintf fmt "(%a :: %a)" pp_exp a pp_exp b
  | Constr (c, ts, e) ->
      Format.fprintf fmt "Constr(%s, [%a], %a)" c
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp_typ)
        ts pp_exp e
  | Pack (c, args) ->
      Format.fprintf fmt "%s(%a)" c
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp_exp)
        args
  | Unpack (c, e, i) -> Format.fprintf fmt "Unpack(%s, %a, %d)" c pp_exp e i
  | List es ->
      Format.fprintf fmt "[%a]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           pp_exp)
        es
  | Head e -> Format.fprintf fmt "head %a" pp_exp e
  | Tail e -> Format.fprintf fmt "tail %a" pp_exp e
  | Empty -> Format.fprintf fmt "[]"
  | Fail -> Format.fprintf fmt "Fail"

let pp_constr_def fmt (c, ts) =
  match ts with
  | [] -> Format.fprintf fmt "%s" c
  | _ ->
      Format.fprintf fmt "%s of %a" c
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " * ")
           pp_typ)
        ts

let pp_def fmt d =
  match d with
  | DLet (v, e) -> Format.fprintf fmt "let %s = %a" v pp_exp e
  | DDef (f, vs, e) ->
      Format.fprintf fmt "def %s %s = %a" f (String.concat " " vs) pp_exp e
  | DDefrec (f, vs, e) ->
      Format.fprintf fmt "defrec %s %s = %a" f (String.concat " " vs) pp_exp e
  | DType (n, cs) ->
      Format.fprintf fmt "type %s = %a" n
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " | ")
           pp_constr_def)
        cs

let pp_prog fmt (Prog (defs, e)) =
  Format.fprintf fmt "Prog([%a], %a)"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
       pp_def)
    defs pp_exp e
