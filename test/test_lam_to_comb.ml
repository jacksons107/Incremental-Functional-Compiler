open Lam
open Comb
open Lam_to_comb

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


let () = print_endline (pp_comb (lam_to_comb prog))

