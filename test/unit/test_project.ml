open Compiler

(* Exercises Project.root_of: walking upward from a file's path to the
   nearest ancestor directory containing a .oj-project marker. *)

let dir_perm = 0o755

let touch path =
  let oc = open_out path in
  close_out oc

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir dir_perm
  end

let fresh_dir () =
  let dir =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "oj_project_test_%d" (Random.bits ()))
  in
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

let suite = [ check_found; check_missing ]
