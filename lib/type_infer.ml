open Elam
open Ast

type typ = 
    | TInt
    | TBool
    | TLam of typ * typ
    | TList of typ
    | TConstr of string
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

        | TConstr c1, TConstr c2 when c1 = c2 -> ()

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

let rec typedef_helper tname args = match args with
    | []    -> TConstr tname
    | x::xs -> TLam (x, typedef_helper tname xs)

(* TODO -- support lists in constructors *)
let rec args_to_typs args = match args with
    | [] -> []
    | x::xs -> match x with
        | "int" -> TInt::(args_to_typs xs)
        | "bool" -> TBool::(args_to_typs xs)
        | _      -> TConstr x::(args_to_typs xs)

let rec setup_env typedefs env = match typedefs with
    | [] -> env
    | TypeDef (t, c, a)::xs -> 
        let new_typ = typedef_helper t (args_to_typs a) in
        let new_env = Env.add c new_typ env in
        setup_env xs new_env

let rec infer expr env = match expr with
    | EInt _  -> TInt
    | EBool _ -> TBool
    | EFail   -> fresh_var ()
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

    | EIsCons
    | EIsConstr ->
        let fresh = fresh_var () in 
        TLam (fresh, TBool) 

    | EY ->
        let fresh = fresh_var () in
        TLam (TLam (fresh, fresh), fresh)

    (* TODO -- prevent multiple types with same constructor *)
    | EConstr (cname, _) -> 
        lookup env cname

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

