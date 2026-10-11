open Compiler
open Ast
open J_machine

(* Exercises Store.write/Store.read: a hash's entry round-trips through
   its on-disk directory unchanged. *)

let fresh_root () =
  let root =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "oj_store_test_%d" (Random.bits ()))
  in
  Sys.mkdir root Store.store_dir;
  root

let check =
  Alcotest.test_case "a written entry reads back identically" `Quick
    (fun () ->
      let root = fresh_root () in
      let hash = Hash.of_string "deadbeef" in
      let instrs = [ INT 5; ID "one" ] in
      let scheme = Forall (IntSet.empty, TInt) in
      let ast = DLet ("x", Int 5) in
      Store.write ~root hash instrs scheme ast;
      let instrs', scheme', ast' = Store.read ~root hash in
      Alcotest.(check (list (Alcotest.testable pp_instr ( = ))))
        "instrs round-trip" instrs instrs';
      Alcotest.(check bool) "scheme round-trips" true (scheme = scheme');
      Alcotest.(check bool) "ast round-trips" true (ast = ast'))

let suite = [ check ]
