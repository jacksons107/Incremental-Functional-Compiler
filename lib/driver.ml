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

(* Walks a Prog's definitions in source order, type-checking and compiling
   each one on its own and caching the result under the content hash of
   its own body. Also threads hash_env (name -> hash) through the walk,
   extending it with each definition's name and hash. Returns the final
   type environment, hash_env, and cache.

   TODO: constructors are hashed via Serialize.encode on their LConstr
   node, which still serializes the constructor's name as a literal
   string (see the TODO in serialize.ml) rather than its position within
   a canonicalized, SCC-grouped type-definition group. Real structural
   constructor hashing is parked in PLAN.md. *)
let inspect_defs defs =
  let typedefs = get_types defs in
  let type_env = setup_env typedefs empty_env in
  let compile_elam elam =
    let lam = elam_to_lam elam in
    (lam, comb_to_j (lam_to_comb lam))
  in
  let hash_of lam hash_env =
    let free_map name = List.assoc name hash_env in
    Digest.to_hex (Digest.string (Serialize.encode ~free_map lam))
  in
  let rec walk defs env hash_env acc =
    match defs with
    | [] -> (env, List.rev hash_env, List.rev acc)
    | DType (_, constrs) :: rest ->
        let hash_env', acc' =
          List.fold_left
            (fun (hash_env, acc) (cname, arg_typs) ->
              let lam, instrs =
                compile_elam (EConstr (cname, List.length arg_typs))
              in
              let hash = hash_of lam hash_env in
              ((cname, hash) :: hash_env, (hash, instrs) :: acc))
            (hash_env, acc) constrs
        in
        walk rest env hash_env' acc'
    | d :: rest ->
        let name, elam = def_to_elam d in
        let ty = infer elam env in
        let scheme = generalize ty env in
        let env' = Env.add name scheme env in
        let lam, instrs = compile_elam elam in
        let hash = hash_of lam hash_env in
        walk rest env' ((name, hash) :: hash_env) ((hash, instrs) :: acc)
  in
  walk defs type_env [] []

module StringSet = Set.Make (String)

let free_refs instrs =
  List.filter_map (function ID name -> Some name | _ -> None) instrs

(* Starting from the entry point's own free-variable references, pulls in
   whatever those names need too, transitively -- rather than a separate
   dependency-collection pass, a definition's dependency set is exactly
   the ID occurrences already sitting in its compiled j_instr list. Each
   name is translated to its hash via hash_env before touching cache,
   since cache is keyed by hash, not name. *)
let dependancy_hashes entry_instrs cache hash_env =
  let rec go frontier seen =
    match frontier with
    | [] -> seen
    | name :: rest ->
        let hash = List.assoc name hash_env in
        if StringSet.mem hash seen then go rest seen
        else
          let deps = free_refs (List.assoc hash cache) in
          go (deps @ rest) (StringSet.add hash seen)
  in
  go (free_refs entry_instrs) StringSet.empty

(* Assembly/linking: the entry point is the program's trailing expression,
   compiled the same way as any top-level definition's body, against the
   final type environment the walk accumulated. Only the definitions the
   entry point transitively needs get included -- cache is already in a
   valid dependency order (source order), so filtering it down to the
   needed set preserves that order with no separate topological sort.
   Each needed definition's chunk renders its own j_instr list unmodified,
   followed by one extra line (globals_push(stack_pop());) that moves its
   just-built graph off the stack and into its permanent globals[] slot;
   the entry point's own chunk gets no such line, so its result is exactly
   what's left on top of the stack for the runtime's reduce() to pick up.
   The hash -> globals[] index table comes from that same needed-defs
   processing order; resolve first translates an ID's name to its hash
   via hash_env, then looks up that hash's index -- resolving every ID
   directly at render time, same as before. Nothing is ever rendered
   before every ID in it is already resolvable, so there's no
   placeholder/substitution mechanism anywhere. *)
let compile_to_c ~filename ~source =
  let (Prog (defs, exp)) = parse filename source in
  let type_env, hash_env, cache = inspect_defs defs in
  let entry_elam = ast_to_elam exp in
  let _ = infer entry_elam type_env in
  let entry_instrs = comb_to_j (lam_to_comb (elam_to_lam entry_elam)) in
  let needed_hashes = dependancy_hashes entry_instrs cache hash_env in
  let needed_defs =
    List.filter (fun (hash, _) -> StringSet.mem hash needed_hashes) cache
  in
  let indices = List.mapi (fun i (hash, _) -> (hash, i)) needed_defs in
  let resolve name = List.assoc (List.assoc name hash_env) indices in
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
