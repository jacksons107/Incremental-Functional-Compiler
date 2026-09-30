type lam_exp =
  | LVar of string
  | LInt of int
  | LBool of bool
  | LString of string
  | LEq
  | LPlus
  | LIf
  | LHead
  | LTail
  | LCons
  | LEmpty
  | LConstr of string * int
  | LUnpack
  | LIsCons
  | LIsConstr
  | LFail
  | LY
  | LApp of lam_exp * lam_exp
  | Lam of string * lam_exp

let rec pp fmt e =
  match e with
  | LVar x -> Format.fprintf fmt "%s" x
  | LInt n -> Format.fprintf fmt "%d" n
  | LBool b -> Format.fprintf fmt "%b" b
  | LString s -> Format.fprintf fmt "%S" s
  | LEq -> Format.fprintf fmt "=="
  | LPlus -> Format.fprintf fmt "+"
  | LIf -> Format.fprintf fmt "IF"
  | LHead -> Format.fprintf fmt "HEAD"
  | LTail -> Format.fprintf fmt "TAIL"
  | LCons -> Format.fprintf fmt "CONS"
  | LEmpty -> Format.fprintf fmt "[]"
  | LConstr (c, n) -> Format.fprintf fmt "Constr(%s, %d)" c n
  | LUnpack -> Format.fprintf fmt "Unpack"
  | LIsCons -> Format.fprintf fmt "IsCons"
  | LIsConstr -> Format.fprintf fmt "IsConstr"
  | LFail -> Format.fprintf fmt "Fail"
  | LY -> Format.fprintf fmt "Y"
  | LApp (f, a) -> Format.fprintf fmt "(%a %a)" pp f pp a
  | Lam (v, b) -> Format.fprintf fmt "(\\%s -> %a)" v pp b
