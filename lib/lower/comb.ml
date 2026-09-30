type comb_exp =
  | I
  | K
  | S
  | CInt of int
  | CBool of bool
  | CString of string
  | CEq
  | CPlus
  | CIf
  | CHead
  | CTail
  | CCons
  | CEmpty
  | CConstr of string * int
  | CUnpack
  | CIsCons
  | CIsConstr
  | CFail
  | CY
  | CVar of string
  | CApp of comb_exp * comb_exp

let rec pp fmt e =
  match e with
  | I -> Format.fprintf fmt "I"
  | K -> Format.fprintf fmt "K"
  | S -> Format.fprintf fmt "S"
  | CInt n -> Format.fprintf fmt "%d" n
  | CBool b -> Format.fprintf fmt "%b" b
  | CString s -> Format.fprintf fmt "%S" s
  | CEq -> Format.fprintf fmt "=="
  | CPlus -> Format.fprintf fmt "+"
  | CIf -> Format.fprintf fmt "IF"
  | CHead -> Format.fprintf fmt "HEAD"
  | CTail -> Format.fprintf fmt "TAIL"
  | CCons -> Format.fprintf fmt "CONS"
  | CEmpty -> Format.fprintf fmt "[]"
  | CConstr (c, n) -> Format.fprintf fmt "Constr(%s, %d)" c n
  | CUnpack -> Format.fprintf fmt "Unpack"
  | CIsCons -> Format.fprintf fmt "IsCons"
  | CIsConstr -> Format.fprintf fmt "IsConstr"
  | CFail -> Format.fprintf fmt "Fail"
  | CY -> Format.fprintf fmt "Y"
  | CVar x -> Format.fprintf fmt "%s" x
  | CApp (f, a) -> Format.fprintf fmt "(%a %a)" pp f pp a
