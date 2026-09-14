open Compiler

let () =
  if Array.length Sys.argv <> 2 then (
    prerr_endline "Usage: build <file.oj>";
    exit 1
  );

  let filename = Sys.argv.(1) in
  if not (Filename.check_suffix filename ".oj") then (
    prerr_endline "Error: input file must have .oj extension";
    exit 1
  );

  (* Read program from file *)
  let program =
    let ch = open_in filename in
    let len = in_channel_length ch in
    let s = really_input_string ch len in
    close_in ch;
    s
  in

  let entry = Driver.compile_to_c ~filename ~source:program in
  Driver.emit_c_file ~out_path:"generated.c" ~entry_body:entry;
  match Driver.build_exe ~c_file:"generated.c" ~runtime_dir:"." ~out_exe:"prog" with
  | Ok () -> Printf.printf "Build successful. Run ./prog\n"
  | Error msg -> Printf.eprintf "%s\n" msg
