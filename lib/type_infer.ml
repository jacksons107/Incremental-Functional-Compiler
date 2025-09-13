open Elam
open Ast

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
        | TInt, TInt       -> ()
        | TBool, TBool     -> ()
        | TString, TString -> ()
    
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

let rec unpack_helper s_typ idx =
    match prune s_typ with
        | TLam (t, rest) ->
            if idx = 0 then t
        else unpack_helper rest (idx - 1)
        | _ ->
    raise (Type_error "unpack_helper: index out of range or wrong type")

let rec setup_env typedefs env = match typedefs with
    | [] -> env
    | TypeDef (t, c, a)::xs -> 
        let new_typ = typedef_helper t a in
        let new_env = Env.add c new_typ env in
        setup_env xs new_env

let rec infer expr env = match expr with
    | EInt _  -> 
        (* let () = print_endline "INT" in *)
        TInt
    | EBool _ -> 
        (* let () = print_endline "BOOl" in *)
        TBool
    | EString _ ->
        (* let () = print_endline "STRING" in *)
        TString
    | EFail   -> 
        (* let () = print_endline "FAIL" in *)
        fresh_var ()
    | EVar x  -> 
        (* let () = print_endline "VAR" in *)
        lookup env x

    | EPlus -> 
        (* let () = print_endline "PLUS" in *)
        TLam (TInt, TLam (TInt, TInt))

    | EIf -> 
        (* let () = print_endline "IF" in *)
        let fresh = fresh_var () in
        TLam (TBool, TLam (fresh, TLam (fresh, fresh)))

    | EEq -> 
        (* let () = print_endline "EQ" in *)
        let fresh = fresh_var () in
        TLam (fresh, TLam (fresh, TBool))

    | ECons ->
        (* let () = print_endline "CONS" in *)
        let fresh = fresh_var () in
        TLam (fresh, TLam (TList fresh, TList fresh))

    | EEmpty ->
        (* let () = print_endline "EMPTY" in *)
        let fresh = fresh_var () in
        TList fresh

    | EHead  ->
        (* let () = print_endline "HEAD" in *)
        let fresh = fresh_var () in
        TLam (TList fresh, fresh)

    | ETail ->
        (* let () = print_endline "TAIL" in *)
        let fresh = fresh_var () in
        TLam (TList fresh, TList fresh)

    | EIsCons ->
        (* let () = print_endline "ISCONS" in *)
        let fresh = fresh_var () in 
        TLam (fresh, TBool) 

    | EIsConstr ->
        (* let () = print_endline "ISCONSTR" in *)
        let fresh1 = fresh_var () in 
        let fresh2 = fresh_var () in
        TLam (fresh1, TLam (fresh2, TBool))

    | EY ->
        (* let () = print_endline "Y" in *)
        let fresh = fresh_var () in
        TLam (TLam (fresh, fresh), fresh)

    (* TODO -- prevent multiple types with same constructor *)
    | EConstr (cname, _) -> 
        (* let () = print_endline "CONSTR" in *)
        lookup env cname

    | EUnpack (cname, _, idx) ->
        (* let () = print_endline "UNPACK" in *)
        let constr_typ = lookup env cname in
        unpack_helper constr_typ idx

    | ELam (v, b) ->
        (* let () = print_endline "LAM" in *)
        let fresh = fresh_var () in
        let new_env = Env.add v fresh env in
        TLam (fresh, infer b new_env)

    | EApp (f, a) -> 
        (* let () = print_endline "APP" in *)
        let f_typ = infer f env in
        let a_typ = infer a env in
        let fresh = fresh_var () in
        unify f_typ (TLam (a_typ, fresh));
        prune fresh

    | ELet (v, e, b) ->
        (* let () = print_endline "LET" in *)
        let e_typ = infer e env in
        let new_env = Env.add v e_typ env in
        infer b new_env
