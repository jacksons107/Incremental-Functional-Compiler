(* A project's name -> hash namespace: an append-only log, one
   "name<TAB>hash" line per write. A name's current hash is whichever
   line for it appears last in the file. *)

let parse_line line =
  match String.index_opt line '\t' with
  | None -> failwith ("Namespace.read: malformed line: " ^ line)
  | Some i ->
      let name = String.sub line 0 i in
      let hash = String.sub line (i + 1) (String.length line - i - 1) in
      (name, Hash.of_string hash)

let read (path : string) : Hash.t Ordered_map.Name_map.t =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | line -> loop (parse_line line :: acc)
    | exception End_of_file -> List.rev acc
  in
  let entries = loop [] in
  close_in ic;
  Ordered_map.Name_map.of_list entries

(* Creates a new, empty namespace file at path. *)
let create (path : string) : unit =
  let oc = open_out path in
  close_out oc

let rw_rw_rw = 0o666

let append (path : string) (entries : (string * Hash.t) list) : unit =
  let oc = open_out_gen [ Open_append; Open_wronly ] rw_rw_rw path in
  List.iter
    (fun (name, hash) -> Printf.fprintf oc "%s\t%s\n" name (Hash.to_string hash))
    entries;
  close_out oc
