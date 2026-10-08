open Compiler
open Ast
open J_machine

(* Exercises Driver.inspect_defs: the hash-keyed bottom-up walk that
   type-checks and compiles each top-level definition on its own, caching
   its j_instr list (with ID <name> for every free variable) under the
   hash of its own body rather than its name, so that two
   differently-named, identically-bodied definitions share one cache
   entry. *)

let instr = Alcotest.testable pp_instr ( = )
let entries = Alcotest.(list (pair string (list instr)))

(* Reconstructs a name-keyed view for assertions, via the same two-step
   lookup compile_to_c's own `resolve` does: a name -> hash via hash_env,
   then hash -> compiled instructions via cache. *)
let compile src =
  let (Prog (defs, _)) = Driver.parse "test.oj" src in
  let _, hash_env, cache = Driver.inspect_defs defs in
  List.map (fun (name, hash) -> (name, List.assoc hash cache)) hash_env

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
        let _, hash_env, _ = Driver.inspect_defs defs in
        Alcotest.(check string)
          "a and b share a hash" (List.assoc "a" hash_env)
          (List.assoc "b" hash_env));
  ]
