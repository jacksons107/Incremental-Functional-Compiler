(* Finds a project's root: the nearest ancestor directory (including the
   starting directory itself) that contains a .oj-project marker file. *)

let marker_name = ".oj-project"

exception No_project_root of string

let has_marker dir = Sys.file_exists (Filename.concat dir marker_name)

let to_absolute path =
  if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
  else path

(* path is any file within the project, e.g. a scratch .oj file. *)
let root_of (path : string) : string =
  let rec walk dir =
    if has_marker dir then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then raise (No_project_root path) else walk parent
  in
  walk (Filename.dirname (to_absolute path))

let oj_dir root = Filename.concat root ".oj"
let store_path root = Filename.concat (oj_dir root) "store"
let namespace_path root = Filename.concat (oj_dir root) "namespace"

let rwxr_xr_x = 0o755

let rec mkdir_recursive dir =
  if not (Sys.file_exists dir) then begin
    mkdir_recursive (Filename.dirname dir);
    Sys.mkdir dir rwxr_xr_x
  end

let touch path =
  let oc = open_out path in
  close_out oc

exception Already_a_project of string

(* Sets up root as a fresh project: the marker file, an empty store
   directory, and an empty namespace file. *)
let create (root : string) : unit =
  if has_marker root then raise (Already_a_project root)
  else begin
    mkdir_recursive root;
    touch (Filename.concat root marker_name);
    mkdir_recursive (store_path root);
    Namespace.create (namespace_path root)
  end
