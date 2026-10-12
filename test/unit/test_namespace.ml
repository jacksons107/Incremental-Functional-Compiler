open Compiler

(* Exercises Namespace.create/append/read: create makes a new empty
   namespace file; append (which assumes the file already exists) adds
   to its append-only log; read folds it to each name's most recently
   appended hash. *)

let fresh_path () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "oj_namespace_test_%d" (Random.bits ()))

let as_strings entries =
  List.map
    (fun (n, h) -> (n, Hash.to_string h))
    (Ordered_map.Name_map.to_list entries)

let check_create_then_read =
  Alcotest.test_case "create makes a new, empty, readable namespace file"
    `Quick (fun () ->
      let path = fresh_path () in
      Namespace.create path;
      Alcotest.(check (list (pair string string)))
        "no entries" [] (as_strings (Namespace.read path)))

let check_roundtrip =
  Alcotest.test_case "entries round-trip through a single append" `Quick
    (fun () ->
      let path = fresh_path () in
      Namespace.create path;
      Namespace.append path
        [
          ("two", Hash.of_string "bbb");
          ("one", Hash.of_string "aaa");
          ("three", Hash.of_string "ccc");
        ];
      Alcotest.(check (list (pair string string)))
        "order of first appearance"
        [ ("two", "bbb"); ("one", "aaa"); ("three", "ccc") ]
        (as_strings (Namespace.read path)))

let check_read_missing_raises =
  Alcotest.test_case "reading a file that was never created raises" `Quick
    (fun () ->
      let raised =
        try
          ignore (Namespace.read (fresh_path ()));
          false
        with Sys_error _ -> true
      in
      Alcotest.(check bool) "raised Sys_error" true raised)

let check_append_missing_raises =
  Alcotest.test_case
    "appending to a file that was never created raises, rather than \
     creating it"
    `Quick (fun () ->
      let path = fresh_path () in
      let raised =
        try
          Namespace.append path [ ("x", Hash.of_string "h") ];
          false
        with Sys_error _ -> true
      in
      Alcotest.(check bool) "raised Sys_error" true raised;
      Alcotest.(check bool) "file still doesn't exist" false
        (Sys.file_exists path))

let check_later_append_wins =
  Alcotest.test_case
    "appending a name again later makes that the current value, without \
     losing the earlier line"
    `Quick (fun () ->
      let path = fresh_path () in
      Namespace.create path;
      Namespace.append path [ ("x", Hash.of_string "old") ];
      Namespace.append path [ ("x", Hash.of_string "new") ];
      Alcotest.(check (list (pair string string)))
        "one current entry, the latest" [ ("x", "new") ]
        (as_strings (Namespace.read path)))

let suite =
  [
    check_create_then_read;
    check_roundtrip;
    check_read_missing_raises;
    check_append_missing_raises;
    check_later_append_wins;
  ]
