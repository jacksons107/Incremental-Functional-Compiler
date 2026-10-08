open Lam

(* Tag bytes are assigned explicitly and must never be reassigned or reused --
   they are part of the canonical, hash-affecting encoding. Adding a new case
   later should claim the next unused number, not renumber existing ones. *)
let tag_lvar = 0
let tag_lint = 1
let tag_lbool = 2
let tag_lstring = 3
let tag_leq = 4
let tag_lplus = 5
let tag_lif = 6
let tag_lhead = 7
let tag_ltail = 8
let tag_lcons = 9
let tag_lempty = 10
let tag_lconstr = 11
let tag_lunpack = 12
let tag_lis_cons = 13
let tag_lis_constr = 14
let tag_lfail = 15
let tag_ly = 16
let tag_lapp = 17
let tag_lam = 18
let tag_lvar_free = 19
let add_tag buf t = Buffer.add_uint8 buf t
let add_i64 buf n = Buffer.add_int64_be buf (Int64.of_int n)
let add_bool buf b = Buffer.add_uint8 buf (if b then 1 else 0)

let add_string buf s =
  add_i64 buf (String.length s);
  Buffer.add_string buf s

exception Unbound_free_variable of string

(* For callers that don't expect any free variables -- hitting one is a
   bug in the caller, not something to resolve silently. *)
let no_free_vars x = raise (Unbound_free_variable x)

(* [scope] is the list of bound names, innermost binder first. Returns
   [Some depth] if [name] is bound, where [depth] counts how many binders
   out it is (0 = the innermost), or [None] if [name] is free. *)
let rec bound_name_index name scope =
  match scope with
  | [] -> None
  | x :: rest ->
      if x = name then Some 0
      else Option.map (fun i -> i + 1) (bound_name_index name rest)

let rec serialize buf scope free_map (e : lam_exp) =
  match e with
  | LVar x -> (
      match bound_name_index x scope with
      | Some i ->
          add_tag buf tag_lvar;
          add_i64 buf i
      | None ->
          add_tag buf tag_lvar_free;
          add_string buf (free_map x))
  | LInt n ->
      add_tag buf tag_lint;
      add_i64 buf n
  | LBool b ->
      add_tag buf tag_lbool;
      add_bool buf b
  | LString s ->
      add_tag buf tag_lstring;
      add_string buf s
  | LEq -> add_tag buf tag_leq
  | LPlus -> add_tag buf tag_lplus
  | LIf -> add_tag buf tag_lif
  | LHead -> add_tag buf tag_lhead
  | LTail -> add_tag buf tag_ltail
  | LCons -> add_tag buf tag_lcons
  | LEmpty -> add_tag buf tag_lempty
  | LConstr (c, a) ->
      add_tag buf tag_lconstr;
      add_string buf c;
      (*TODO Will need to positionally encode the constructor name*)
      add_i64 buf a
  | LUnpack -> add_tag buf tag_lunpack
  | LIsCons -> add_tag buf tag_lis_cons
  | LIsConstr -> add_tag buf tag_lis_constr
  | LFail -> add_tag buf tag_lfail
  | LY -> add_tag buf tag_ly
  | LApp (e1, e2) ->
      add_tag buf tag_lapp;
      serialize buf scope free_map e1;
      serialize buf scope free_map e2
  | Lam (v, body) ->
      add_tag buf tag_lam;
      serialize buf (v :: scope) free_map body

let encode ~free_map (e : lam_exp) : string =
  let buf = Buffer.create 32 in
  serialize buf [] free_map e;
  Buffer.contents buf
