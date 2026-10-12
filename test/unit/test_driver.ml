open Compiler
open Ast
open J_machine

(* Exercises Driver.inspect_defs: type-checks and compiles each
   top-level definition on its own, keyed by the hash of its own body. *)

let instr = Alcotest.testable pp_instr ( = )
let entries = Alcotest.(list (pair string (list instr)))

let no_seed = Ordered_map.Name_map.empty

let hash_for name name_to_hash =
  Option.get (Ordered_map.Name_map.find_opt name name_to_hash)

let instrs_for hash hash_to_instrs =
  Option.get (Ordered_map.Hash_map.find_opt hash hash_to_instrs)

(* Reconstructs a name-keyed view for assertions. *)
let compile src =
  let (Prog (defs, _)) = Driver.parse "test.oj" src in
  let _, name_to_hash, hash_to_instrs, _ =
    Driver.inspect_defs ~name_to_hash:no_seed defs
  in
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
        let _, name_to_hash, _, _ =
          Driver.inspect_defs ~name_to_hash:no_seed defs
        in
        Alcotest.(check string)
          "a and b share a hash"
          (Hash.to_string (hash_for "a" name_to_hash))
          (Hash.to_string (hash_for "b" name_to_hash)));
    Alcotest.test_case "recovers a simple let's source" `Quick (fun () ->
        let (Prog (defs, _)) = Driver.parse "test.oj" "let x = 5;\nx" in
        let _, name_to_hash, _, hash_to_def =
          Driver.inspect_defs ~name_to_hash:no_seed defs
        in
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
        let _, name_to_hash, _, hash_to_def =
          Driver.inspect_defs ~name_to_hash:no_seed defs
        in
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
        let _, name_to_hash, _, hash_to_def =
          Driver.inspect_defs ~name_to_hash:no_seed defs
        in
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
        let _, fresh_name_to_hash, _, _ =
          Driver.inspect_defs ~name_to_hash:no_seed fresh_defs
        in
        Alcotest.(check string)
          "persisted hash matches recompiling \"let x = 2\" alone"
          (Hash.to_string (hash_for "x" fresh_name_to_hash))
          (Hash.to_string (hash_for "x" persisted)));
  ]
