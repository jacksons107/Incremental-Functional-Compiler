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
