type elam_exp =
  | EVar of string
  | EInt of int
  | EBool of bool
  | EString of string
  | EEq
  | EPlus
  | EIf
  | EY
  | EHead
  | ETail
  | ECons
  | EEmpty
  | EConstr of string * int
  | EUnpack of string * elam_exp * int
  | EIsCons
  | EIsConstr
  | EFail
  | EApp of elam_exp * elam_exp
  | ELam of string * elam_exp
  | ELet of string * elam_exp * elam_exp

let rec pp fmt e =
  match e with
  | EVar x -> Format.fprintf fmt "%s" x
  | EInt n -> Format.fprintf fmt "%d" n
  | EBool b -> Format.fprintf fmt "%b" b
  | EString s -> Format.fprintf fmt "%S" s
  | EEq -> Format.fprintf fmt "=="
  | EPlus -> Format.fprintf fmt "+"
  | EIf -> Format.fprintf fmt "IF"
  | EY -> Format.fprintf fmt "Y"
  | EHead -> Format.fprintf fmt "HEAD"
  | ETail -> Format.fprintf fmt "TAIL"
  | ECons -> Format.fprintf fmt "CONS"
  | EEmpty -> Format.fprintf fmt "[]"
  | EConstr (c, n) -> Format.fprintf fmt "Constr(%s, %d)" c n
  | EUnpack (c, e, i) -> Format.fprintf fmt "Unpack(%s, %a, %d)" c pp e i
  | EIsCons -> Format.fprintf fmt "IsCons"
  | EIsConstr -> Format.fprintf fmt "IsConstr"
  | EFail -> Format.fprintf fmt "Fail"
  | EApp (f, a) -> Format.fprintf fmt "(%a %a)" pp f pp a
  | ELam (v, b) -> Format.fprintf fmt "(\\%s -> %a)" v pp b
  | ELet (v, b, e) -> Format.fprintf fmt "let %s = %a in %a" v pp b pp e
