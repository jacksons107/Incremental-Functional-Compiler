open Compiler
module M = Ordered_map.Make (String)

(* Exercises Ordered_map.Make's invariants: at most one entry per key,
   insertion order preserved, overwriting a key keeps its position. *)

let check_empty_is_empty =
  Alcotest.test_case "empty has no entries" `Quick (fun () ->
      Alcotest.(check (list (pair string int))) "empty" [] (M.to_list M.empty))

let check_set_appends_new_key =
  Alcotest.test_case "setting new keys preserves insertion order" `Quick
    (fun () ->
      let t = M.empty |> M.set "a" 1 |> M.set "b" 2 |> M.set "c" 3 in
      Alcotest.(check (list (pair string int)))
        "insertion order"
        [ ("a", 1); ("b", 2); ("c", 3) ]
        (M.to_list t))

let check_overwrite_keeps_position =
  Alcotest.test_case
    "re-setting an existing key updates its value without moving it" `Quick
    (fun () ->
      let t = M.empty |> M.set "a" 1 |> M.set "b" 2 |> M.set "a" 99 in
      Alcotest.(check (list (pair string int)))
        "a stays first, with the new value"
        [ ("a", 99); ("b", 2) ]
        (M.to_list t))

let check_find_opt =
  Alcotest.test_case "find_opt distinguishes present from absent keys" `Quick
    (fun () ->
      let t = M.set "a" 1 M.empty in
      Alcotest.(check (option int)) "present" (Some 1) (M.find_opt "a" t);
      Alcotest.(check (option int)) "absent" None (M.find_opt "b" t))

let check_of_list_dedups_last_wins =
  Alcotest.test_case
    "of_list keeps one entry per key, with the later occurrence winning"
    `Quick (fun () ->
      let t = M.of_list [ ("a", 1); ("b", 2); ("a", 3) ] in
      Alcotest.(check (list (pair string int)))
        "a keeps its first slot but its last value"
        [ ("a", 3); ("b", 2) ]
        (M.to_list t))

let check_diff =
  Alcotest.test_case
    "diff reports only keys that are new or changed, not unchanged ones"
    `Quick (fun () ->
      let old_t = M.of_list [ ("a", 1); ("b", 2) ] in
      let new_t = M.of_list [ ("a", 1); ("b", 99); ("c", 3) ] in
      Alcotest.(check (list (pair string int)))
        "b changed, c is new, a untouched"
        [ ("b", 99); ("c", 3) ]
        (M.diff old_t new_t))

let suite =
  [
    check_empty_is_empty;
    check_set_appends_new_key;
    check_overwrite_keeps_position;
    check_find_opt;
    check_of_list_dedups_last_wins;
    check_diff;
  ]
