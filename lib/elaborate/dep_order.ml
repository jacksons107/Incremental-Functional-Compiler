open Ast

exception Mutual_recursion of string list

let rec pat_vars p =
  match p with
  | PVar v -> [ v ]
  | PInt _ | PBool _ | PEmpty -> []
  | PCons (a, b) -> pat_vars a @ pat_vars b
  | PConstr (_, ps) -> List.concat_map pat_vars ps

(* Collects every name e references that isn't currently in bound --
   a lambda/def/defrec parameter or match-pattern variable introduced
   within e itself. Constructor names (Pack, Constr, Unpack, IsConstr)
   are always collected, since constructor names live in a separate,
   unshadowable namespace from VAR-bound names. *)
let rec global_refs bound e =
  match e with
  | Var x -> if List.mem x bound then [] else [ x ]
  | Int _ | Bool _ | Empty | Fail -> []
  | Eq (a, b) | Plus (a, b) | App (a, b) | Cons (a, b) ->
      global_refs bound a @ global_refs bound b
  | IsCons e | Head e | Tail e -> global_refs bound e
  | IsConstr (e, c) -> c :: global_refs bound e
  | Let (v, b, cont) -> global_refs bound b @ global_refs (v :: bound) cont
  | Def (f, vs, b, cont) ->
      global_refs (vs @ bound) b @ global_refs (f :: bound) cont
  | Defrec (f, vs, b, cont) ->
      global_refs (f :: vs @ bound) b @ global_refs (f :: bound) cont
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

(* The name(s) a top-level def introduces -- one for DLet/DDef/DDefrec,
   one per constructor for DType. *)
let def_provides = function
  | DLet (v, _) -> [ v ]
  | DDef (f, _, _) -> [ f ]
  | DDefrec (f, _, _) -> [ f ]
  | DType (_, constrs) -> List.map fst constrs

(* The other top-level names a def's own body references, excluding
   anything the def provides itself -- a defrec's self-reference (or a
   plain def's invalid one) is never a real ordering dependency, since
   neither is resolved by looking the name up among other definitions. *)
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

(* Reorders defs into a valid dependency order via DFS: a def's
   dependencies are visited, and so appended to the result, before the
   def itself. Raises Mutual_recursion if DFS revisits a def that's
   still on the active path -- a cycle spanning two or more distinct
   defs, since def_requires already excludes self-references. *)
let topo_sort (defs : def list) : def list =
  let defs_arr = Array.of_list defs in
  let n = Array.length defs_arr in
  let name_to_idx = Hashtbl.create 16 in
  Array.iteri
    (fun i d ->
      List.iter (fun name -> Hashtbl.replace name_to_idx name i) (def_provides d))
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
