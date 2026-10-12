open Compiler

(* Exercises Project.root_of: walking upward from a file's path to the
   nearest ancestor directory containing a .oj-project marker. *)

let touch path =
  let oc = open_out path in
  close_out oc

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir Project.rwxr_xr_x
  end

let fresh_path () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "oj_project_test_%d" (Random.bits ()))

let fresh_dir () =
  let dir = fresh_path () in
  mkdir_p dir;
  dir

let check_found =
  Alcotest.test_case "finds the marked root from a file several directories deep"
    `Quick (fun () ->
      let root = fresh_dir () in
      touch (Filename.concat root ".oj-project");
      let nested = List.fold_left Filename.concat root [ "a"; "b"; "c" ] in
      mkdir_p nested;
      let scratch = Filename.concat nested "scratch.oj" in
      Alcotest.(check string) "root" root (Project.root_of scratch))

let check_missing =
  Alcotest.test_case "raises when no marker exists above the given path" `Quick
    (fun () ->
      let dir = fresh_dir () in
      let scratch = Filename.concat dir "scratch.oj" in
      Alcotest.check_raises "no project root"
        (Project.No_project_root scratch) (fun () ->
          ignore (Project.root_of scratch)))

let check_create =
  Alcotest.test_case "create sets up the marker, store dir, and namespace file"
    `Quick (fun () ->
      let root = fresh_path () in
      Project.create root;
      Alcotest.(check bool)
        "marker exists" true
        (Sys.file_exists (Filename.concat root ".oj-project"));
      Alcotest.(check bool)
        "store dir exists" true
        (Sys.file_exists (Project.store_path root));
      Alcotest.(check bool)
        "namespace file exists" true
        (Sys.file_exists (Project.namespace_path root));
      Alcotest.(check int)
        "namespace starts empty" 0
        (List.length
           (Ordered_map.Name_map.to_list
              (Namespace.read (Project.namespace_path root)))))

let check_create_twice =
  Alcotest.test_case "creating an already-a-project dir raises" `Quick
    (fun () ->
      let root = fresh_path () in
      Project.create root;
      Alcotest.check_raises "already a project"
        (Project.Already_a_project root) (fun () -> Project.create root))

let check_independent_stores =
  Alcotest.test_case "two separate projects get independent store paths"
    `Quick (fun () ->
      let root_a = fresh_path () in
      let root_b = fresh_path () in
      Project.create root_a;
      Project.create root_b;
      Alcotest.(check bool)
        "store paths differ" true
        (Project.store_path root_a <> Project.store_path root_b))

let check_store_wiring =
  Alcotest.test_case "Store round-trips through Project.store_path" `Quick
    (fun () ->
      let root = fresh_path () in
      Project.create root;
      let hash = Hash.of_string "deadbeef" in
      let instrs = [ J_machine.INT 5 ] in
      let scheme = Ast.Forall (Ast.IntSet.empty, Ast.TInt) in
      let ast = Ast.DLet ("x", Ast.Int 5) in
      Store.write ~root:(Project.store_path root) hash instrs scheme ast;
      let instrs', _, _ = Store.read ~root:(Project.store_path root) hash in
      Alcotest.(check (list (Alcotest.testable J_machine.pp_instr ( = ))))
        "instrs round-trip" instrs instrs')

let suite =
  [
    check_found;
    check_missing;
    check_create;
    check_create_twice;
    check_independent_stores;
    check_store_wiring;
  ]
