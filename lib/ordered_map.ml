(* An insertion-ordered map: to_list preserves insertion order instead
   of sorting by key, and overwriting an existing key updates its value
   in place rather than moving it. *)
module Make (Ord : Map.OrderedType) = struct
  type key = Ord.t
  type 'a t = (key * 'a) list

  let empty : 'a t = []

  let mem (k : key) (t : 'a t) : bool =
    List.exists (fun (k', _) -> Ord.compare k k' = 0) t

  let set (k : key) (v : 'a) (t : 'a t) : 'a t =
    if mem k t then
      List.map
        (fun (k', v') -> if Ord.compare k k' = 0 then (k, v) else (k', v'))
        t
    else t @ [ (k, v) ]

  let find_opt (k : key) (t : 'a t) : 'a option =
    List.find_opt (fun (k', _) -> Ord.compare k k' = 0) t |> Option.map snd

  let to_list (t : 'a t) : (key * 'a) list = t

  let of_list (entries : (key * 'a) list) : 'a t =
    List.fold_left (fun acc (k, v) -> set k v acc) empty entries

  (* Entries in new_t that are absent from old_t or have a different value. *)
  let diff (old_t : 'a t) (new_t : 'a t) : (key * 'a) list =
    List.filter
      (fun (k, v) ->
        match find_opt k old_t with Some v' -> v <> v' | None -> true)
      new_t
end

module Name_map = Make (String)
module Hash_map = Make (Hash)
