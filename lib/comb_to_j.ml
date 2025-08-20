open Comb
open J_machine

(* TODO -- factor out pp_comb to utils *)
let rec pp_comb cexp = match cexp with
  | CPlus         -> "+"
  | CEq           -> "=="
  | CIf           -> "IF"
  | CHead         -> "HEAD"
  | CTail         -> "TAIL"
  | CY            -> "Y"
  | CInt n        -> string_of_int n
  | CBool b       -> string_of_bool b
  | CVar v        -> "CVar " ^ v
  | I             -> "I"
  | K             -> "K"
  | S             -> "S"
  | CCons         -> "CONS"
  | CEmpty        -> "[]"
  | CIsEmpty      -> "IsEmpty"
  | CIsCons       -> "IsCons"
  | CIsInt        -> "IsInt"
  | CFail         -> "Fail"
  | CApp (f, a)   ->
      let pf = match f with CApp _ -> "(" ^ pp_comb f ^ ")" | _ -> pp_comb f in
      let pa = match a with CApp _ -> "(" ^ pp_comb a ^ ")" | _ -> pp_comb a in
      pf ^ " " ^ pa

let rec comb_to_j exp = match exp with
    | CInt n         -> [INT n]
    | CBool b        -> [BOOL b]
    | CEmpty         -> [EMPTY]
    | CFail          -> [FAIL]
    | CCons          -> [GLOBAL (2, CONS)]
    | CHead          -> [GLOBAL (1, HEAD)]
    | CTail          -> [GLOBAL (1, TAIL)]
    | CEq            -> [GLOBAL (1, EQ)]
    | CIsEmpty       -> [GLOBAL (1, ISEMPTY)]
    | CIsCons        -> [GLOBAL (1, ISCONS)]
    | CIsInt         -> [GLOBAL (1, ISINT)]
    | CPlus          -> [GLOBAL (2, ADD)]
    | CIf            -> [GLOBAL (3, IF)]
    | CY             -> [GLOBAL (1, Y)]
    | I              -> [GLOBAL (1, I)]
    | K              -> [GLOBAL (2, K)]
    | S              -> [GLOBAL (3, S)]
    | CApp (e1, e2)  -> comb_to_j e2 @ comb_to_j e1 @ [APP]
    | _              -> failwith (pp_comb exp)

