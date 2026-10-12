open Compiler
open Ast
open J_machine

(* Exercises Driver.inspect_defs: type-checks and compiles each
   top-level definition on its own, keyed by the hash of its own body. *)

let instr = Alcotest.testable pp_instr ( = )
let entries = Alcotest.(list (pair string (list instr)))

let no_project_seed =
  ( Type_infer.empty_env,
    Ordered_map.Name_map.empty,
    Ordered_map.Hash_map.empty,
    Ordered_map.Hash_map.empty )

let no_store_lookup (_ : Hash.t) : (j_instr list * tyscheme * def) option =
  None

let no_store_save (_ : Hash.t) (_ : j_instr list) (_ : tyscheme) (_ : def) :
    unit =
  ()

let inspect defs =
  Driver.inspect_defs ~seed:no_project_seed ~store_lookup:no_store_lookup
    ~store_save:no_store_save defs

let hash_for name name_to_hash =
  Option.get (Ordered_map.Name_map.find_opt name name_to_hash)

let instrs_for hash hash_to_instrs =
  Option.get (Ordered_map.Hash_map.find_opt hash hash_to_instrs)

(* Reconstructs a name-keyed view for assertions. *)
let compile src =
  let (Prog (defs, _)) = Driver.parse "test.oj" src in
  let _, name_to_hash, hash_to_instrs, _ = inspect defs in
  List.map
    (fun (name, hash) -> (name, instrs_for hash hash_to_instrs))
    (Ordered_map.Name_map.to_list name_to_hash)

let check name src expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check entries name expected (compile src))

let suite =
  [
    check "a def with no free variables compiles with no ID" "let x = 5;"
      [ ("x", [ INT 5 ]) ];
    check "a second def referencing the first compiles to an ID for it"
      "let one = 1;\nlet two = one;"
      [ ("one", [ INT 1 ]); ("two", [ ID "one" ]) ];
    Alcotest.test_case
      "a def with its own argument still records the dependency as an ID among \
       its compiled instructions"
      `Quick (fun () ->
        let result = compile "let one = 1;\ndef inc x = x + one;" in
        Alcotest.(check (list string))
          "names, in source order" [ "one"; "inc" ] (List.map fst result);
        Alcotest.(check bool)
          "inc's instructions mention ID \"one\"" true
          (List.mem (ID "one") (List.assoc "inc" result)));
    check
      "a DType with no other defs contributes one entry per constructor, not \
       one for the type itself"
      "type pair = Pair of int * int;"
      [ ("Pair", [ CONSTR (2, "Pair") ]) ];
    Alcotest.test_case
      "a later def's use of a constructor records it as a dependency, same as \
       any other free variable"
      `Quick (fun () ->
        let result =
          compile "type pair = Pair of int * int;\ndef mk a b = Pair (a, b);"
        in
        Alcotest.(check (list string))
          "names, in source order" [ "Pair"; "mk" ] (List.map fst result);
        Alcotest.(check bool)
          "mk's instructions mention ID \"Pair\"" true
          (List.mem (ID "Pair") (List.assoc "mk" result)));
    Alcotest.test_case
      "referencing an undefined name raises the same Type_error infer already \
       raises for an unbound variable"
      `Quick (fun () ->
        Alcotest.check_raises "unbound"
          (Type_infer.Type_error "Unbound variable: undefined") (fun () ->
            ignore (compile "let bad = undefined;")));
    Alcotest.test_case
      "two differently-named definitions with identical bodies hash to the \
       same cache entry"
      `Quick (fun () ->
        let (Prog (defs, _)) =
          Driver.parse "test.oj" "let a = 5;\nlet b = 5;"
        in
        let _, name_to_hash, _, _ = inspect defs in
        Alcotest.(check string)
          "a and b share a hash"
          (Hash.to_string (hash_for "a" name_to_hash))
          (Hash.to_string (hash_for "b" name_to_hash)));
    Alcotest.test_case "recovers a simple let's source" `Quick (fun () ->
        let (Prog (defs, _)) = Driver.parse "test.oj" "let x = 5;\nx" in
        let _, name_to_hash, _, hash_to_def = inspect defs in
        Alcotest.(check string)
          "recovered source" "let x = 5"
          (Recover.recover_source name_to_hash hash_to_def "x"));
    Alcotest.test_case
      "recovers a def referencing a forward-declared dependency, using the \
       dependency's own name"
      `Quick (fun () ->
        let (Prog (defs, _)) =
          Driver.parse "test.oj" "def inc x = x + one;\nlet one = 1;\ninc 2"
        in
        let _, name_to_hash, _, hash_to_def = inspect defs in
        Alcotest.(check string)
          "recovered source" "def inc x = (x + one)"
          (Recover.recover_source name_to_hash hash_to_def "inc"));
    Alcotest.test_case
      "a defrec's self-calls recover under whichever name is queried, not a \
       different alias sharing the same hash"
      `Quick (fun () ->
        let (Prog (defs, _)) =
          Driver.parse "test.oj"
            "defrec fact n = if n == 0 then 1 else n + fact (n + -1);\n\
             defrec fact2 n = if n == 0 then 1 else n + fact2 (n + -1);\n\
             fact 5"
        in
        let _, name_to_hash, _, hash_to_def = inspect defs in
        Alcotest.(check string)
          "fact and fact2 share a hash"
          (Hash.to_string (hash_for "fact" name_to_hash))
          (Hash.to_string (hash_for "fact2" name_to_hash));
        Alcotest.(check string)
          "fact's self-call recovers as \"fact\""
          "defrec fact n = if (n == 0) then 1 else (n + (fact (n + -1)))"
          (Recover.recover_source name_to_hash hash_to_def "fact");
        Alcotest.(check string)
          "fact2's self-call recovers as \"fact2\", not \"fact\""
          "defrec fact2 n = if (n == 0) then 1 else (n + (fact2 (n + -1)))"
          (Recover.recover_source name_to_hash hash_to_def "fact2"));
    Alcotest.test_case
      "inspect_defs_in_project seeds from the namespace and preserves \
       untouched entries on write-back"
      `Quick (fun () ->
        let root =
          Filename.concat (Filename.get_temp_dir_name ())
            (Printf.sprintf "oj_driver_test_%d" (Random.bits ()))
        in
        Project.create root;
        let (Prog (defs1, _)) = Driver.parse "test.oj" "let one = 1;" in
        ignore (Driver.inspect_defs_in_project ~root defs1);
        let (Prog (defs2, _)) = Driver.parse "test.oj" "let two = 2;" in
        ignore (Driver.inspect_defs_in_project ~root defs2);
        let names =
          List.map fst
            (Ordered_map.Name_map.to_list
               (Namespace.read (Project.namespace_path root)))
        in
        Alcotest.(check (list string))
          "both compiles' names persisted" [ "one"; "two" ] names);
    Alcotest.test_case
      "inspect_defs_in_project's write-back updates a redefined name's hash \
       instead of duplicating it"
      `Quick (fun () ->
        let root =
          Filename.concat (Filename.get_temp_dir_name ())
            (Printf.sprintf "oj_driver_test_%d" (Random.bits ()))
        in
        Project.create root;
        let (Prog (defs1, _)) = Driver.parse "test.oj" "let x = 1;" in
        ignore (Driver.inspect_defs_in_project ~root defs1);
        let (Prog (defs2, _)) = Driver.parse "test.oj" "let x = 2;" in
        ignore (Driver.inspect_defs_in_project ~root defs2);
        let persisted = Namespace.read (Project.namespace_path root) in
        Alcotest.(check int) "one entry for x" 1
          (List.length (Ordered_map.Name_map.to_list persisted));
        let (Prog (fresh_defs, _)) = Driver.parse "test.oj" "let x = 2;" in
        let _, fresh_name_to_hash, _, _ = inspect fresh_defs in
        Alcotest.(check string)
          "persisted hash matches recompiling \"let x = 2\" alone"
          (Hash.to_string (hash_for "x" fresh_name_to_hash))
          (Hash.to_string (hash_for "x" persisted)));
    Alcotest.test_case
      "a scratch file that only references a name (never defines it) \
       compiles successfully, using that name's store-cached scheme and \
       instrs"
      `Quick (fun () ->
        let root =
          Filename.concat (Filename.get_temp_dir_name ())
            (Printf.sprintf "oj_driver_test_%d" (Random.bits ()))
        in
        Project.create root;
        let (Prog (defs1, _)) = Driver.parse "test.oj" "let one = 1;" in
        ignore (Driver.inspect_defs_in_project ~root defs1);
        let (Prog (defs2, _)) =
          Driver.parse "test.oj" "def inc x = x + one;"
        in
        let _, name_to_hash, hash_to_instrs, _ =
          Driver.inspect_defs_in_project ~root defs2
        in
        let incs_instrs =
          instrs_for (hash_for "inc" name_to_hash) hash_to_instrs
        in
        Alcotest.(check bool)
          "inc's instructions mention ID \"one\"" true
          (List.mem (ID "one") incs_instrs));
    Alcotest.test_case
      "recompiling an identical definition in a second, separate call hits \
       the store instead of writing it again"
      `Quick (fun () ->
        let root =
          Filename.concat (Filename.get_temp_dir_name ())
            (Printf.sprintf "oj_driver_test_%d" (Random.bits ()))
        in
        Project.create root;
        let (Prog (defs1, _)) = Driver.parse "test.oj" "let one = 1;" in
        let _, name_to_hash1, _, _ =
          Driver.inspect_defs_in_project ~root defs1
        in
        let (Prog (defs2, _)) = Driver.parse "test.oj" "let one = 1;" in
        (* If the hit path were broken (always a miss), this would raise
           Sys_error trying to re-create an already-existing store
           directory for the same hash. *)
        let _, name_to_hash2, _, _ =
          Driver.inspect_defs_in_project ~root defs2
        in
        Alcotest.(check string)
          "same hash both times"
          (Hash.to_string (hash_for "one" name_to_hash1))
          (Hash.to_string (hash_for "one" name_to_hash2)));
    Alcotest.test_case
      "inspect_defs_in_project refuses a DType rather than persisting a \
       constructor with no backing store entry"
      `Quick (fun () ->
        let root =
          Filename.concat (Filename.get_temp_dir_name ())
            (Printf.sprintf "oj_driver_test_%d" (Random.bits ()))
        in
        Project.create root;
        let (Prog (defs, _)) =
          Driver.parse "test.oj" "type pair = Pair of int * int;"
        in
        Alcotest.check_raises "constructors not supported in a project"
          Driver.Constructors_not_supported_in_project (fun () ->
            ignore (Driver.inspect_defs_in_project ~root defs)));
    Alcotest.test_case
      "a dependency shared by two dependents is compiled exactly once"
      `Quick (fun () ->
        let (Prog (defs, _)) =
          Driver.parse "test.oj"
            "let base = 1;\n\
             def left x = x + base;\n\
             def right x = x + base + 1;\n\
             def top x = left x + right x;"
        in
        let save_counts = Hashtbl.create 8 in
        let counting_store_save hash _ _ _ =
          let count = Option.value (Hashtbl.find_opt save_counts hash) ~default:0 in
          Hashtbl.replace save_counts hash (count + 1)
        in
        let _, name_to_hash, _, _ =
          Driver.inspect_defs ~seed:no_project_seed
            ~store_lookup:no_store_lookup ~store_save:counting_store_save defs
        in
        List.iter
          (fun name ->
            Alcotest.(check int)
              (name ^ " compiled exactly once") 1
              (Hashtbl.find save_counts (hash_for name name_to_hash)))
          [ "base"; "left"; "right"; "top" ]);
  ]
