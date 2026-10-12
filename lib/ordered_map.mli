module Make (Ord : Map.OrderedType) : sig
  type key = Ord.t
  type 'a t

  val empty : 'a t
  val set : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val to_list : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
  val diff : 'a t -> 'a t -> (key * 'a) list
end

module Name_map : sig
  type key = string
  type 'a t

  val empty : 'a t
  val set : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val to_list : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
  val diff : 'a t -> 'a t -> (key * 'a) list
end

module Hash_map : sig
  type key = Hash.t
  type 'a t

  val empty : 'a t
  val set : key -> 'a -> 'a t -> 'a t
  val find_opt : key -> 'a t -> 'a option
  val to_list : 'a t -> (key * 'a) list
  val of_list : (key * 'a) list -> 'a t
  val diff : 'a t -> 'a t -> (key * 'a) list
end
