open Ast 

(* DType -> Constr chain *)
let rec constr_chain dtype expr = match dtype with
    | DType (n, cs) -> (match cs with
        | []    -> expr
        | (cname, args)::xs -> 
            Constr (cname, args, constr_chain (DType (n, xs)) expr))
    | _ -> failwith "constr_chain on non-dtype"

let rec def_to_exp (Prog (defs, expr)) = match defs with
    | [] -> expr
    | x::xs -> 
        match x with
            | DLet (v, e)        -> Let (v, e, def_to_exp (Prog (xs, expr)))
            | DDef (f, vs, e)    -> Def (f, vs, e, def_to_exp (Prog (xs, expr)))
            | DDefrec (f, vs, e) -> Defrec (f, vs, e, def_to_exp (Prog (xs, expr)))
            (* | DType (n, c, a) -> Type (n, c, a, def_to_exp (Prog (xs, expr))) *)
            | DType _            -> constr_chain x (def_to_exp (Prog (xs, expr)))
            | DConstr _          -> failwith "def_to_exp on dconstr"

(* DType -> DConstr list *)
let rec get_constrs dtype = match dtype with
    | DType (tname, cs) -> (match cs with
        | [] -> []
        | (cname, args)::xs -> 
            TypeDef (tname, cname, args) :: get_constrs (DType (tname, xs)))
    | _ -> failwith "get_constrs on non-dtype"

let rec get_types defs = match defs with
    | [] -> []
    | x::xs -> match x with 
        | DType _ -> get_constrs x @ get_types xs
        | _       -> get_types xs
