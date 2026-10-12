(* A project's name -> hash namespace: a plain text file, one
   "name<TAB>hash" pair per line, sorted by name. *)

let parse_line line =
  match String.index_opt line '\t' with
  | None -> failwith ("Namespace.read: malformed line: " ^ line)
  | Some i ->
      let name = String.sub line 0 i in
      let hash = String.sub line (i + 1) (String.length line - i - 1) in
      (name, Hash.of_string hash)

let read (path : string) : (string * Hash.t) list =
  let ic = open_in path in
  let rec loop acc =
    match input_line ic with
    | line -> loop (parse_line line :: acc)
    | exception End_of_file -> List.rev acc
  in
  let entries = loop [] in
  close_in ic;
  entries

let write (path : string) (entries : (string * Hash.t) list) : unit =
  let sorted = List.sort (fun (a, _) (b, _) -> String.compare a b) entries in
  let oc = open_out path in
  List.iter
    (fun (name, hash) -> Printf.fprintf oc "%s\t%s\n" name (Hash.to_string hash))
    sorted;
  close_out oc
