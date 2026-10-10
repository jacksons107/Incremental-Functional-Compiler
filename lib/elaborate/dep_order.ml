open Ast

exception Mutual_recursion of string list

let rec pat_vars p =
  match p with
  | PVar v -> [ v ]
  | PInt _ | PBool _ | PEmpty -> []
  | PCons (a, b) -> pat_vars a @ pat_vars b
  | PConstr (_, ps) -> List.concat_map pat_vars ps

(* Every name e references that isn't in bound (a locally-bound
   parameter or pattern variable). Constructor names are always
   collected, regardless of bound. *)
let rec global_refs bound e =
  match e with
  | Var x -> if List.mem x bound then [] else [ x ]
  | Ref _ | Int _ | Bool _ | Empty | Fail -> []
  | Eq (a, b) | Plus (a, b) | App (a, b) | Cons (a, b) ->
      global_refs bound a @ global_refs bound b
  | IsCons e | Head e | Tail e -> global_refs bound e
  | IsConstr (e, c) -> c :: global_refs bound e
  | Let (v, b, cont) -> global_refs bound b @ global_refs (v :: bound) cont
  | Def (f, vs, b, cont) ->
      global_refs (vs @ bound) b @ global_refs (f :: bound) cont
  | Defrec (f, vs, b, cont) ->
      global_refs ((f :: vs) @ bound) b @ global_refs (f :: bound) cont
  | Match (scruts, cases) ->
      List.concat_map (global_refs bound) scruts
      @ List.concat_map
          (fun (pats, rhs) ->
            let bound' = List.concat_map pat_vars pats @ bound in
            global_refs bound' rhs)
          cases
  | If (c, t, e) ->
      global_refs bound c @ global_refs bound t @ global_refs bound e
  | Constr (c, _, e) -> c :: global_refs bound e
  | Pack (c, args) -> c :: List.concat_map (global_refs bound) args
  | Unpack (c, e, _) -> c :: global_refs bound e
  | List es -> List.concat_map (global_refs bound) es

(* Rewrites e, replacing every unshadowed Var x with Ref h wherever
   resolve x = Some h. *)
let rec anonymize resolve bound e =
  let go = anonymize resolve bound in
  match e with
  | Var x -> (
      if List.mem x bound then e
      else match resolve x with Some h -> Ref h | None -> e)
  | Ref _ | Int _ | Bool _ | Empty | Fail -> e
  | Eq (a, b) -> Eq (go a, go b)
  | Plus (a, b) -> Plus (go a, go b)
  | App (a, b) -> App (go a, go b)
  | Cons (a, b) -> Cons (go a, go b)
  | IsCons e -> IsCons (go e)
  | Head e -> Head (go e)
  | Tail e -> Tail (go e)
  | IsConstr (e, c) -> IsConstr (go e, c)
  | Let (v, b, cont) -> Let (v, go b, anonymize resolve (v :: bound) cont)
  | Def (f, vs, b, cont) ->
      Def
        ( f,
          vs,
          anonymize resolve (vs @ bound) b,
          anonymize resolve (f :: bound) cont )
  | Defrec (f, vs, b, cont) ->
      Defrec
        ( f,
          vs,
          anonymize resolve ((f :: vs) @ bound) b,
          anonymize resolve (f :: bound) cont )
  | Match (scruts, cases) ->
      Match
        ( List.map go scruts,
          List.map
            (fun (pats, rhs) ->
              let bound' = List.concat_map pat_vars pats @ bound in
              (pats, anonymize resolve bound' rhs))
            cases )
  | If (c, t, e) -> If (go c, go t, go e)
  | Constr (c, ts, e) -> Constr (c, ts, go e)
  | Pack (c, args) -> Pack (c, List.map go args)
  | Unpack (c, e, i) -> Unpack (c, go e, i)
  | List es -> List (List.map go es)

(* The name(s) a top-level def introduces. *)
let def_provides = function
  | DLet (v, _) -> [ v ]
  | DDef (f, _, _) -> [ f ]
  | DDefrec (f, _, _) -> [ f ]
  | DType (_, constrs) -> List.map fst constrs

(* The other top-level names a def's body references, excluding
   anything the def provides itself. *)
let def_requires d =
  let provides = def_provides d in
  let raw =
    match d with
    | DLet (_, e) -> global_refs [] e
    | DDef (_, vs, e) -> global_refs vs e
    | DDefrec (_, vs, e) -> global_refs vs e
    | DType (_, _) -> []
  in
  List.sort_uniq compare (List.filter (fun r -> not (List.mem r provides)) raw)

(* Reorders defs so each one follows its own dependencies (DFS-based
   topological sort). Raises Mutual_recursion on a cycle. *)
let topo_sort (defs : def list) : def list =
  let defs_arr = Array.of_list defs in
  let n = Array.length defs_arr in
  let name_to_idx = Hashtbl.create 16 in
  Array.iteri
    (fun i d ->
      List.iter
        (fun name -> Hashtbl.replace name_to_idx name i)
        (def_provides d))
    defs_arr;
  let state = Array.make n `Unvisited in
  let order = ref [] in
  let rec visit path i =
    match state.(i) with
    | `Done -> ()
    | `OnPath ->
        let cycle =
          List.rev_map (fun j -> List.hd (def_provides defs_arr.(j))) (i :: path)
        in
        raise (Mutual_recursion cycle)
    | `Unvisited ->
        state.(i) <- `OnPath;
        List.iter
          (fun dep_name ->
            match Hashtbl.find_opt name_to_idx dep_name with
            | Some j -> visit (i :: path) j
            | None -> ())
          (def_requires defs_arr.(i));
        state.(i) <- `Done;
        order := defs_arr.(i) :: !order
  in
  for i = 0 to n - 1 do
    visit [] i
  done;
  List.rev !order
