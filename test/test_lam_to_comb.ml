open Lam
open Comb
open Lam_to_comb

let rec pp_comb = function
  | I             -> "I"
  | K             -> "K"
  | S             -> "S"
  | CPlus         -> "+"
  | CIf           -> "IF"
  | CHead         -> "HEAD"
  | CTail         -> "TAIL"
  | CY            -> "Y"
  | CInt n        -> string_of_int n
  | CBool b       -> string_of_bool b
  | CVar v        -> v
  | CCons         -> "CONS"
  | CEmpty        -> "[]"
  | CApp (f, a)   ->
      let pf = match f with CApp _ -> "(" ^ pp_comb f ^ ")" | _ -> pp_comb f in
      let pa = match a with CApp _ -> "(" ^ pp_comb a ^ ")" | _ -> pp_comb a in
      pf ^ " " ^ pa


(* (+ 2 3) *)
(* let add23 = (LApp ((LApp (LPlus, (LInt 2))), (LInt 3))) *)
(* (+ x x) *)
let double = (LApp ((LApp (LPlus, (LVar "x"))), (LVar "x")))
(* (lambda (x) (+ x x)) *)
let lam_double = (Lam ("x", double))
(* ((lambda (x) (+ x x)) 5) *)
let prog = (LApp (lam_double, LInt 5))
(* ((lambda (x) (+ x x)) (+ 2 3)) *)
(* let prog = (LApp (lam_double, add23)) *)

let () = print_endline ("(" ^ (pp_comb (lam_to_comb prog)) ^ ")")
