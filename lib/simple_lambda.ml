type lam_exp = 
    | LVar of string
    | LInt of int
    | LPlus
    | LApp of lam_exp * lam_exp
    | Lam of string * lam_exp

type comb_exp =
    | I | K | S
    | CInt of int
    | CPlus
    | CVar of string
    | CApp of comb_exp * comb_exp

let rec abstract var exp = match exp with
    | CVar y when var = y -> I
    | CVar _ 
    | I | K | S           
    | CPlus               -> CApp (K, exp)
    | CInt n              -> CApp (K, CInt n)
    | CApp (e1, e2)       -> CApp (CApp (S, (abstract var e1)), (abstract var e2))

let rec lam_to_comb exp = match exp with
    | LVar x          -> CVar x
    | LInt n          -> CInt n
    | LPlus           -> CPlus
    | LApp (e1, e2)   -> CApp (lam_to_comb e1, lam_to_comb e2)
    | Lam (var, body) -> abstract var (lam_to_comb body)


let rec pp_comb = function
  | I             -> "I"
  | K             -> "K"
  | S             -> "S"
  | CPlus         -> "+"
  | CInt n        -> string_of_int n
  | CVar v        -> v
  | CApp (f, a)   ->
      let pf = match f with CApp _ -> "(" ^ pp_comb f ^ ")" | _ -> pp_comb f in
      let pa = match a with CApp _ -> "(" ^ pp_comb a ^ ")" | _ -> pp_comb a in
      pf ^ " " ^ pa

(* (+ x x) *)
let double = (LApp ((LApp (LPlus, (LVar "x"))), (LVar "x")))
(* (lambda (x) (+ x 3)) *)
let lam_double = (Lam ("x", double))
(* ((lambda (x) (+ x 3)) 4) *)
let prog = (LApp (lam_double, (LInt 5)))

(* 
lam_to_comb (lambda (x) (+ x 3))
abstract x (+ x 3)
S ()


*)