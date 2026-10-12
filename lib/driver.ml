open Ast
open Desugar
open Elam
open Ast_to_elam
open Type_infer
open Elam_to_lam
open Lam_to_comb
open Comb_to_j
open J_machine

let print_position outx lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  Printf.fprintf outx "File \"%s\", line %d, column %d" pos.Lexing.pos_fname
    pos.Lexing.pos_lnum
    (pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1)

let parse filename s =
  let lexbuf = Lexing.from_string s in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try Parser.prog Lexer.read lexbuf
  with Parser.Error ->
    Printf.eprintf "%a: syntax error\n" print_position lexbuf;
    exit 1

let typecheck ~filename ~source =
  let (Prog (defs, exp)) = parse filename source in
  let ast_exp = def_to_exp (Prog (defs, exp)) in
  let typedefs = get_types defs in
  let env = setup_env typedefs empty_env in
  let elam = ast_to_elam ast_exp in
  infer elam env

(* Reorders defs (Dep_order.topo_sort), then type-checks and compiles
   each one, keyed by the hash of its own body rather than its name.
   Returns the type environment, name_to_hash, hash_to_instrs, and
   hash_to_def.

   name_to_hash seeds the walk with entries already known (e.g. from a
   project's persisted namespace), so a def can resolve against a name
   that isn't in defs itself.

   TODO: constructor hashing is nominal, not structural -- see the TODO
   in serialize.ml. *)
let inspect_defs ~name_to_hash defs =
  let defs = Dep_order.topo_sort defs in
  let typedefs = get_types defs in
  let type_env = setup_env typedefs empty_env in
  let compile_elam elam =
    let lam = elam_to_lam elam in
    (lam, comb_to_j (lam_to_comb lam))
  in
  let hash_of lam (name_to_hash : Hash.t Ordered_map.Name_map.t) : Hash.t =
    let free_map (name : string) : string =
      Hash.to_string (Option.get (Ordered_map.Name_map.find_opt name name_to_hash))
    in
    Hash.of_string
      (Digest.to_hex (Digest.string (Serialize.encode ~free_map lam)))
  in
  let rec walk defs env (name_to_hash : Hash.t Ordered_map.Name_map.t)
      (hash_to_instrs : j_instr list Ordered_map.Hash_map.t)
      (hash_to_def : def Ordered_map.Hash_map.t) =
    match defs with
    | [] -> (env, name_to_hash, hash_to_instrs, hash_to_def)
    | DType (_, constrs) :: rest ->
        let name_to_hash', hash_to_instrs' =
          List.fold_left
            (fun (name_to_hash, hash_to_instrs) (cname, arg_typs) ->
              let lam, instrs =
                compile_elam (EConstr (cname, List.length arg_typs))
              in
              let (hash : Hash.t) = hash_of lam name_to_hash in
              ( Ordered_map.Name_map.set cname hash name_to_hash,
                Ordered_map.Hash_map.set hash instrs hash_to_instrs ))
            (name_to_hash, hash_to_instrs)
            constrs
        in
        walk rest env name_to_hash' hash_to_instrs' hash_to_def
    | d :: rest ->
        let name, elam = def_to_elam d in
        let ty = infer elam env in
        let scheme = generalize ty env in
        let env' = Env.add name scheme env in
        let lam, instrs = compile_elam elam in
        let (hash : Hash.t) = hash_of lam name_to_hash in
        let resolve (n : string) : Hash.t option =
          if n = name then Some hash
          else Ordered_map.Name_map.find_opt n name_to_hash
        in
        let anonymized_def =
          match d with
          | DLet (n, e) -> DLet (n, Dep_order.anonymize resolve [] e)
          | DDef (n, vs, e) -> DDef (n, vs, Dep_order.anonymize resolve vs e)
          | DDefrec (n, vs, e) ->
              DDefrec (n, vs, Dep_order.anonymize resolve vs e)
          | DType _ -> assert false
        in
        walk rest env'
          (Ordered_map.Name_map.set name hash name_to_hash)
          (Ordered_map.Hash_map.set hash instrs hash_to_instrs)
          (Ordered_map.Hash_map.set hash anonymized_def hash_to_def)
  in
  walk defs type_env name_to_hash Ordered_map.Hash_map.empty
    Ordered_map.Hash_map.empty

(* Seeds inspect_defs's walk from root's persisted namespace, then
   appends whichever names are new or changed back to it. *)
let inspect_defs_in_project ~root defs =
  let seed = Namespace.read (Project.namespace_path root) in
  let result = inspect_defs ~name_to_hash:seed defs in
  let _, name_to_hash, _, _ = result in
  Namespace.append (Project.namespace_path root)
    (Ordered_map.Name_map.diff seed name_to_hash);
  result

module HashSet = Set.Make (Hash)

let free_refs instrs =
  List.filter_map (function ID name -> Some name | _ -> None) instrs

(* The hashes entry_instrs transitively needs, found by translating each
   ID's name to its hash via name_to_hash and following hash_to_instrs. *)
let dependancy_hashes entry_instrs
    (hash_to_instrs : j_instr list Ordered_map.Hash_map.t)
    (name_to_hash : Hash.t Ordered_map.Name_map.t) : HashSet.t =
  let rec go frontier seen =
    match frontier with
    | [] -> seen
    | name :: rest ->
        let (hash : Hash.t) =
          Option.get (Ordered_map.Name_map.find_opt name name_to_hash)
        in
        if HashSet.mem hash seen then go rest seen
        else
          let deps =
            free_refs
              (Option.get (Ordered_map.Hash_map.find_opt hash hash_to_instrs))
          in
          go (deps @ rest) (HashSet.add hash seen)
  in
  go (free_refs entry_instrs) HashSet.empty

(* Assembly/linking: renders the entry point and every definition it
   transitively needs to C text. resolve translates an ID's name to its
   hash, then that hash's globals[] index. *)
let compile_to_c ~filename ~source =
  let (Prog (defs, exp)) = parse filename source in
  let type_env, name_to_hash, hash_to_instrs, _ =
    inspect_defs ~name_to_hash:Ordered_map.Name_map.empty defs
  in
  let entry_elam = ast_to_elam exp in
  let _ = infer entry_elam type_env in
  let entry_instrs = comb_to_j (lam_to_comb (elam_to_lam entry_elam)) in
  let needed_hashes =
    dependancy_hashes entry_instrs hash_to_instrs name_to_hash
  in
  let needed_defs =
    List.filter
      (fun (hash, _) -> HashSet.mem hash needed_hashes)
      (Ordered_map.Hash_map.to_list hash_to_instrs)
  in
  let hash_to_index : (Hash.t * int) list =
    List.mapi (fun i (hash, _) -> (hash, i)) needed_defs
  in
  let resolve (name : string) : int =
    List.assoc
      (Option.get (Ordered_map.Name_map.find_opt name name_to_hash))
      hash_to_index
  in
  let def_chunk (_, instrs) =
    build_graph resolve instrs ^ "\nglobals_push(stack_pop());"
  in
  String.concat "\n"
    (List.map def_chunk needed_defs @ [ build_graph resolve entry_instrs ])

let emit_c_file ~out_path ~entry_body =
  let oc = open_out out_path in
  output_string oc "#include \"runtime.h\"\n";
  output_string oc "void entry() {\n";
  output_string oc (entry_body ^ "\n");
  output_string oc "}";
  close_out oc

let build_exe ~c_file ~runtime_dir ~out_exe =
  let runtime_c = Filename.concat runtime_dir "runtime.c" in
  let utils_c = Filename.concat runtime_dir "utils.c" in
  let cmd =
    Printf.sprintf "gcc -o %s %s %s %s -I%s" (Filename.quote out_exe)
      (Filename.quote c_file) (Filename.quote runtime_c)
      (Filename.quote utils_c)
      (Filename.quote runtime_dir)
  in
  match Sys.command cmd with
  | 0 -> Ok ()
  | n -> Error (Printf.sprintf "gcc failed with code %d" n)
