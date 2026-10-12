open Compiler

(* Exercises Namespace.write/Namespace.read: the name -> hash mapping
   round-trips, sorted by name regardless of write order. *)

let fresh_path () =
  Filename.concat (Filename.get_temp_dir_name ())
    (Printf.sprintf "oj_namespace_test_%d" (Random.bits ()))

let check_roundtrip =
  Alcotest.test_case "entries round-trip sorted by name" `Quick (fun () ->
      let path = fresh_path () in
      let entries =
        [
          ("two", Hash.of_string "bbb");
          ("one", Hash.of_string "aaa");
          ("three", Hash.of_string "ccc");
        ]
      in
      Namespace.write path entries;
      let result = Namespace.read path in
      Alcotest.(check (list (pair string string)))
        "sorted by name"
        [ ("one", "aaa"); ("three", "ccc"); ("two", "bbb") ]
        (List.map (fun (n, h) -> (n, Hash.to_string h)) result))

let check_missing =
  Alcotest.test_case "reading a missing namespace file raises" `Quick
    (fun () ->
      let raised =
        try
          ignore (Namespace.read (fresh_path ()));
          false
        with Sys_error _ -> true
      in
      Alcotest.(check bool) "raised Sys_error" true raised)

let suite = [ check_roundtrip; check_missing ]
