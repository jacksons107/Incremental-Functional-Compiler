open Ast

(* Finds a name currently mapping to hash in name_to_hash. *)
let name_for_hash (name_to_hash : (string * Hash.t) list) (h : Hash.t) : string
    =
  fst (List.find (fun (_, h') -> Hash.equal h' h) name_to_hash)

(* Rewrites e, replacing every Ref h with Var (resolve_ref h). *)
let rec resolve_refs (resolve_ref : Hash.t -> string) e =
  let go = resolve_refs resolve_ref in
  match e with
  | Ref h -> Var (resolve_ref h)
  | Var _ | Int _ | Bool _ | Empty | Fail -> e
  | Eq (a, b) -> Eq (go a, go b)
  | Plus (a, b) -> Plus (go a, go b)
  | App (a, b) -> App (go a, go b)
  | Cons (a, b) -> Cons (go a, go b)
  | IsCons e -> IsCons (go e)
  | Head e -> Head (go e)
  | Tail e -> Tail (go e)
  | IsConstr (e, c) -> IsConstr (go e, c)
  | Let (v, b, cont) -> Let (v, go b, go cont)
  | Def (f, vs, b, cont) -> Def (f, vs, go b, go cont)
  | Defrec (f, vs, b, cont) -> Defrec (f, vs, go b, go cont)
  | Match (scruts, cases) ->
      Match (List.map go scruts, List.map (fun (ps, rhs) -> (ps, go rhs)) cases)
  | If (c, t, e) -> If (go c, go t, go e)
  | Constr (c, ts, e) -> Constr (c, ts, go e)
  | Pack (c, args) -> Pack (c, List.map go args)
  | Unpack (c, e, i) -> Unpack (c, go e, i)
  | List es -> List (List.map go es)

(* Renders readable (not byte-identical) source for name. A Ref to
   name's own hash resolves back to name itself, not name_for_hash's
   arbitrary pick -- so a defrec's self-calls recover under the name
   just asked for, not a different alias sharing the same hash. *)
let recover_source (name_to_hash : (string * Hash.t) list)
    (hash_to_def : (Hash.t * def) list) (name : string) : string =
  let (hash : Hash.t) = List.assoc name name_to_hash in
  let stored = List.assoc hash hash_to_def in
  let resolve_ref (h : Hash.t) : string =
    if Hash.equal h hash then name else name_for_hash name_to_hash h
  in
  let renamed =
    match stored with
    | DLet (_, e) -> DLet (name, resolve_refs resolve_ref e)
    | DDef (_, vs, e) -> DDef (name, vs, resolve_refs resolve_ref e)
    | DDefrec (_, vs, e) -> DDefrec (name, vs, resolve_refs resolve_ref e)
    | DType _ -> assert false
  in
  Format.asprintf "%a" pp_def renamed
