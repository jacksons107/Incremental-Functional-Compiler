(* On-disk, content-addressed store: each hash gets its own directory
   under root, holding its j_instr list, tyscheme, and anonymized Ast.def
   as separate Marshal'd files. *)

let store_dir = 0o755

let def_dir root (hash : Hash.t) = Filename.concat root (Hash.to_string hash)

let write_value path v =
  let oc = open_out_bin path in
  Marshal.to_channel oc v [];
  close_out oc

let read_value path =
  let ic = open_in_bin path in
  let v = Marshal.from_channel ic in
  close_in ic;
  v

let write ~root (hash : Hash.t) (instrs : J_machine.j_instr list)
    (scheme : Ast.tyscheme) (ast : Ast.def) : unit =
  let dir = def_dir root hash in
  Sys.mkdir dir store_dir;
  write_value (Filename.concat dir "j_instr") instrs;
  write_value (Filename.concat dir "type") scheme;
  write_value (Filename.concat dir "ast") ast

let read ~root (hash : Hash.t) :
    J_machine.j_instr list * Ast.tyscheme * Ast.def =
  let dir = def_dir root hash in
  let instrs = read_value (Filename.concat dir "j_instr") in
  let scheme = read_value (Filename.concat dir "type") in
  let ast = read_value (Filename.concat dir "ast") in
  (instrs, scheme, ast)
