open Ast 

let rec def_to_exp (Prog (defs, expr)) = match defs with
    | [] -> expr
    | x::xs -> 
        match x with
            | DLet (v, e) -> Let (v, e, def_to_exp (Prog (xs, expr)))
            | DDef (f, vs, e) -> Def (f, vs, e, def_to_exp (Prog (xs, expr)))
            | DDefrec (f, vs, e) -> Defrec (f, vs, e, def_to_exp (Prog (xs, expr)))
            | DType (n, c, a) -> Type (n, c, a, def_to_exp (Prog (xs, expr)))
