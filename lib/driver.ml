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

(* Separate compilation, name-keyed: walks a Prog's definitions in source
   order -- already a valid dependency order, since forward references
   aren't possible and Defrec's self-recursion is closed via the
   Y-combinator trick, so a recursive call is a bound reference, never an
   ID. The *type* environment for every DType anywhere in the program is
   folded in up front (same as typecheck above), since get_types/setup_env
   already scan the whole defs list regardless of position. Each DType
   also still needs one compiled j_instr-list entry per constructor, keyed
   by the constructor's own name -- ast_to_elam's Pack case
   (`app_constr (EVar name) ...`) references a constructor exactly like
   any other free variable, so a later definition's `Pair(a, b)` compiles
   down to an ID "Pair" that the assembly step needs to resolve against
   *something* in this cache, the same as any other name. Each ordinary
   definition is type-checked against the term environment accumulated so
   far -- extending it via the same generalize-and-Env.add logic infer's
   ELet case already uses, just lifted across separate top-level calls --
   then compiled on its own (def_to_elam's extraction) down to a j_instr
   list, with ID <name> standing in for each free variable. Nothing is
   rendered to C text here; that's the assembly/linking step below.
   Returns the final type environment alongside the cache, since the
   assembly step still needs to type-check the program's trailing
   expression (the entry point) against whatever every definition
   contributed. *)
let inspect_defs defs =
  let typedefs = get_types defs in
  let type_env = setup_env typedefs empty_env in
  let compile_elam elam = comb_to_j (lam_to_comb (elam_to_lam elam)) in
  let rec walk defs env acc =
    match defs with
    | [] -> (env, List.rev acc)
    | DType (_, constrs) :: rest ->
        let compiled =
          List.map
            (fun (cname, arg_typs) ->
              (cname, compile_elam (EConstr (cname, List.length arg_typs))))
            constrs
        in
        walk rest env (List.rev_append compiled acc)
    | d :: rest ->
        let name, elam = def_to_elam d in
        let ty = infer elam env in
        let scheme = generalize ty env in
        let env' = Env.add name scheme env in
        walk rest env' ((name, compile_elam elam) :: acc)
  in
  walk defs type_env []

module StringSet = Set.Make (String)

let free_refs instrs =
  List.filter_map (function ID name -> Some name | _ -> None) instrs

(* Starting from the entry point's own free-variable references, pulls in
   whatever those names need too, transitively -- rather than a separate
   dependency-collection pass, a definition's dependency set is exactly
   the ID occurrences already sitting in its compiled j_instr list. *)
let dependancy_names entry_instrs cache =
  let rec go frontier seen =
    match frontier with
    | [] -> seen
    | name :: rest ->
        if StringSet.mem name seen then go rest seen
        else
          let deps = free_refs (List.assoc name cache) in
          go (deps @ rest) (StringSet.add name seen)
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
   The name -> globals[] index table comes from that same needed-defs
   processing order and resolves every ID directly at render time --
   nothing is ever rendered before every ID in it is already resolvable,
   so there's no placeholder/substitution mechanism anywhere. *)
let compile_to_c ~filename ~source =
  let (Prog (defs, exp)) = parse filename source in
  let type_env, cache = inspect_defs defs in
  let entry_elam = ast_to_elam exp in
  let _ = infer entry_elam type_env in
  let entry_instrs = comb_to_j (lam_to_comb (elam_to_lam entry_elam)) in
  let needed_names = dependancy_names entry_instrs cache in
  let needed_defs =
    List.filter (fun (name, _) -> StringSet.mem name needed_names) cache
  in
  let indices = List.mapi (fun i (name, _) -> (name, i)) needed_defs in
  let resolve name = List.assoc name indices in
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
