open Elam

type typ = 
    | TInt
    | TBool
    | TLam of typ * typ
    | TList of typ
    | TVar of tyvar ref

and tyvar = 
    | Unbound of int
    | Link of typ

let fv = ref 0
let fresh_var () = 
    let fv_ref = !fv in
    incr fv;
    TVar (ref (Unbound fv_ref))

exception Type_error of string

(* Follow links of a tyvar to find typ it is equivalent to *)
let rec prune tvar = match tvar with
    | TVar ({contents = Link ty}) -> prune ty
    | ty -> ty

(* TODO -- add line number to type check errors *)
let rec unify t1 t2 = 
    let t1 = prune t1 in
    let t2 = prune t2 in 
    match (t1, t2) with
        | TInt, TInt   -> ()
        | TBool, TBool -> ()
    
        | TLam (v1, b1), TLam (v2, b2) ->
            unify v1 v2;
            unify b1 b2

        | TList t1, TList t2 ->
            unify t1 t2

        | TVar ({contents = Unbound id1}), 
          TVar ({contents = Unbound id2}) when id1 = id2 -> ()
        
        | TVar ({contents = Unbound _} as v), ty
        | ty, TVar ({contents = Unbound _} as v) ->
            v := Link ty

        | _ -> raise (Type_error "Type mismatch")


module Env = Map.Make(String)
type env = typ Env.t
let empty_env = Env.empty
let lookup env x = 
    try Env.find x env 
    with Not_found -> raise (Type_error ("Unbound variable: " ^ x))

let rec infer expr env = match expr with
    | EInt _  -> TInt
    | EBool _ -> TBool
    | EVar x  -> lookup env x
    | EPlus   -> TLam (TInt, TLam (TInt, TInt))

    | EIf -> 
        let fresh = fresh_var () in
        TLam (TBool, TLam (fresh, TLam (fresh, fresh)))

    | EEq -> 
        let fresh = fresh_var () in
        TLam (fresh, TLam (fresh, TBool))

    | ECons ->
        let fresh = fresh_var () in
        TLam (fresh, TLam (TList fresh, TList fresh))

    | EEmpty ->
        let fresh = fresh_var () in
        TList fresh

    | EHead  ->
        let fresh = fresh_var () in
        TLam (TList fresh, fresh)

    | ETail ->
        let fresh = fresh_var () in
        TLam (TList fresh, TList fresh)

    | ELam (v, b) ->
        let fresh = fresh_var () in
        let new_env = Env.add v fresh env in
        TLam (fresh, infer b new_env)

    | EApp (f, a) -> 
        let f_typ = infer f env in
        let a_typ = infer a env in
        let fresh = fresh_var () in
        unify f_typ (TLam (a_typ, fresh));
        prune fresh

    | ELet (v, e, b) ->
        let e_typ = infer e env in
        let new_env = Env.add v e_typ env in
        infer b new_env

    | _       -> failwith "Trying to infer unimplemented expression type"

