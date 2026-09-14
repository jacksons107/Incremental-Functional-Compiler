open Elam
open Ast

let fv = ref 0

let fresh_var () =
  let fv_ref = !fv in
  incr fv;
  TVar (ref (Unbound fv_ref))

exception Type_error of string

(* Follow links of a tyvar to find typ it is equivalent to *)
let rec prune tvar =
  match tvar with TVar { contents = Link ty } -> prune ty | ty -> ty

module Env = Map.Make (String)

type env = tyscheme Env.t

let empty_env = Env.empty

let lookup env x =
  try Env.find x env
  with Not_found -> raise (Type_error ("Unbound variable: " ^ x))

let rec typedef_helper tname args =
  match args with
  | [] -> TConstr tname
  | x :: xs -> TLam (x, typedef_helper tname xs)

let rec unpack_helper s_typ idx =
  match prune s_typ with
  | TLam (t, rest) -> if idx = 0 then t else unpack_helper rest (idx - 1)
  | _ -> raise (Type_error "unpack_helper: index out of range or wrong type")

let rec fv_typ ty =
  match ty with
  | TInt | TBool | TString | TConstr _ -> IntSet.empty
  | TList t -> fv_typ t
  | TLam (a, b) -> IntSet.union (fv_typ a) (fv_typ b)
  | TVar { contents = v } -> (
      match v with Unbound id -> IntSet.singleton id | Link t -> fv_typ t)

let fv_scheme (Forall (ids, ty)) = IntSet.diff (fv_typ ty) ids

let fv_env env =
  Env.fold
    (fun _ scheme acc -> IntSet.union acc (fv_scheme scheme))
    env IntSet.empty

let generalize ty env =
  let fvs = IntSet.diff (fv_typ ty) (fv_env env) in
  Forall (fvs, ty)

module IntMap = Map.Make (Int)

let sub_map ids =
  IntSet.fold (fun id acc -> IntMap.add id (fresh_var ()) acc) ids IntMap.empty

let instantiate (Forall (ids, ty)) =
  let subs = sub_map ids in
  let rec substitute ty subs =
    match ty with
    | TVar { contents = Unbound id } -> (
        try IntMap.find id subs with Not_found -> ty)
    | TVar { contents = Link t } -> substitute t subs
    | TInt | TBool | TString | TConstr _ -> ty
    | TList t -> TList (substitute t subs)
    | TLam (f, a) -> TLam (substitute f subs, substitute a subs)
  in
  substitute ty subs

let rec occurs_in id t2 =
  match t2 with
  | TVar { contents = Unbound id2 } -> id = id2
  | TVar { contents = Link t } -> occurs_in id t
  | TList t -> occurs_in id t
  | TLam (f, a) -> occurs_in id f || occurs_in id a
  | _ -> false

(* TODO -- add line number to type check errors *)
let rec unify t1 t2 =
  let t1 = prune t1 in
  let t2 = prune t2 in
  match (t1, t2) with
  | TInt, TInt -> ()
  | TBool, TBool -> ()
  | TString, TString -> ()
  | TLam (v1, b1), TLam (v2, b2) ->
      unify v1 v2;
      unify b1 b2
  | TList t1, TList t2 -> unify t1 t2
  | TConstr c1, TConstr c2 when c1 = c2 -> ()
  | TVar { contents = Unbound id1 }, TVar { contents = Unbound id2 }
    when id1 = id2 ->
      ()
  | TVar ({ contents = Unbound id } as v), ty
  | ty, TVar ({ contents = Unbound id } as v) ->
      if occurs_in id ty then raise (Type_error "Infinite type")
      else v := Link ty
  | _ -> raise (Type_error "Type mismatch")

let rec setup_env typedefs env =
  match typedefs with
  | [] -> env
  | TypeDef (t, c, a) :: xs ->
      let new_typ = typedef_helper t a in
      let new_env = Env.add c (Forall (IntSet.empty, new_typ)) env in
      setup_env xs new_env

let rec infer expr env =
  match expr with
  | EInt _ ->
      (* let () = print_endline "INT" in *)
      TInt
  | EBool _ ->
      (* let () = print_endline "BOOl" in *)
      TBool
  | EString _ ->
      (* let () = print_endline "STRING" in *)
      TString
  | EFail ->
      (* let () = print_endline "FAIL" in *)
      fresh_var ()
  | EVar x ->
      (* let () = print_endline "VAR" in *)
      instantiate (lookup env x)
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
  | EHead ->
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
      instantiate (lookup env cname)
  | EUnpack (cname, _, idx) ->
      (* let () = print_endline "UNPACK" in *)
      let constr_typ = lookup env cname in
      unpack_helper (instantiate constr_typ) idx
  | ELam (v, b) ->
      (* let () = print_endline "LAM" in *)
      let fresh = fresh_var () in
      let new_env = Env.add v (Forall (IntSet.empty, fresh)) env in
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
      let e_scheme = generalize e_typ env in
      let new_env = Env.add v e_scheme env in
      infer b new_env
