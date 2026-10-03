open Compiler

let usage_msg = "compiler [-o out] [-c] [-t] <file.oj>"
let output_path = ref None
let no_run = ref false
let typecheck_only = ref false
let input_file = ref None

let set_input f =
  match !input_file with
  | None -> input_file := Some f
  | Some _ ->
      prerr_endline "Error: only one input file may be specified";
      exit 1

let speclist =
  [
    ( "-o",
      Arg.String (fun s -> output_path := Some s),
      "Output executable path (default: derived from input filename)" );
    ("-c", Arg.Set no_run, "Build only; don't run the resulting executable");
    ( "--no-run",
      Arg.Set no_run,
      "Build only; don't run the resulting executable" );
    ( "-t",
      Arg.Set typecheck_only,
      "Type-check only; print the program's inferred type and exit" );
  ]

let () =
  Arg.parse speclist set_input usage_msg;

  let filename =
    match !input_file with
    | Some f -> f
    | None ->
        prerr_endline usage_msg;
        exit 1
  in
  if not (Filename.check_suffix filename ".oj") then (
    prerr_endline "Error: input file must have .oj extension";
    exit 1);

  (* Read program from file *)
  let program =
    let ch = open_in filename in
    let len = in_channel_length ch in
    let s = really_input_string ch len in
    close_in ch;
    s
  in

  if !typecheck_only then (
    let ty = Driver.typecheck ~filename ~source:program in
    Format.printf "%a@." Ast.pp_typ ty;
    exit 0);

  let out_exe =
    match !output_path with
    | Some p -> p
    | None -> Filename.remove_extension (Filename.basename filename)
  in
  let run_path =
    if Filename.is_implicit out_exe then
      Filename.concat Filename.current_dir_name out_exe
    else out_exe
  in

  let entry = Driver.compile_to_c ~filename ~source:program in
  Driver.emit_c_file ~out_path:"generated.c" ~entry_body:entry;
  match
    Driver.build_exe ~c_file:"generated.c" ~runtime_dir:"runtime" ~out_exe
  with
  | Error msg ->
      prerr_endline msg;
      exit 1
  | Ok () ->
      if !no_run then Printf.printf "Build successful. Run %s\n" run_path
      else exit (Sys.command (Filename.quote run_path))
