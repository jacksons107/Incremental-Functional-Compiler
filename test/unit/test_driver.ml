open Compiler
open Ast
open J_machine

(* Exercises Driver.inspect_defs: type-checks and compiles each
   top-level definition on its own, keyed by the hash of its own body. *)

let instr = Alcotest.testable pp_instr ( = )
let entries = Alcotest.(list (pair string (list instr)))

(* Reconstructs a name-keyed view for assertions. *)
let compile src =
  let (Prog (defs, _)) = Driver.parse "test.oj" src in
  let _, name_to_hash, hash_to_instrs, _ = Driver.inspect_defs defs in
  List.map
    (fun (name, hash) -> (name, List.assoc hash hash_to_instrs))
    name_to_hash

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
        let _, name_to_hash, _, _ = Driver.inspect_defs defs in
        Alcotest.(check string)
          "a and b share a hash"
          (Hash.to_string (List.assoc "a" name_to_hash))
          (Hash.to_string (List.assoc "b" name_to_hash)));
    Alcotest.test_case "recovers a simple let's source" `Quick (fun () ->
        let (Prog (defs, _)) = Driver.parse "test.oj" "let x = 5;\nx" in
        let _, name_to_hash, _, hash_to_def = Driver.inspect_defs defs in
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
        let _, name_to_hash, _, hash_to_def = Driver.inspect_defs defs in
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
        let _, name_to_hash, _, hash_to_def = Driver.inspect_defs defs in
        Alcotest.(check string)
          "fact and fact2 share a hash"
          (Hash.to_string (List.assoc "fact" name_to_hash))
          (Hash.to_string (List.assoc "fact2" name_to_hash));
        Alcotest.(check string)
          "fact's self-call recovers as \"fact\""
          "defrec fact n = if (n == 0) then 1 else (n + (fact (n + -1)))"
          (Recover.recover_source name_to_hash hash_to_def "fact");
        Alcotest.(check string)
          "fact2's self-call recovers as \"fact2\", not \"fact\""
          "defrec fact2 n = if (n == 0) then 1 else (n + (fact2 (n + -1)))"
          (Recover.recover_source name_to_hash hash_to_def "fact2"));
  ]
