open Ast

type pat_tag =
    | TagVar
    | TagInt of int
    | TagBool of bool
    | TagCons
    | TagEmpty
    | TagConstr of string

let tag_of_pat pat = match pat with
    | PVar _         -> TagVar
    | PInt i         -> TagInt i
    | PBool b        -> TagBool b
    | PCons _        -> TagCons
    | PEmpty         -> TagEmpty
    | PConstr (c, _) -> TagConstr c

module TagOrd = struct
  type t = pat_tag
  let compare = Stdlib.compare
end
module TagMap = Map.Make(TagOrd)

type matrix = Matrix of (pat list * exp) list
type scruts = Scruts of exp list
type partition = Partition of (pat_tag * matrix) list


(* Check if row of matrix mat has all variables as its patterns *)
let inspect_row row = 
    let rec all_vars pats = match pats with
        | []    -> true
        | x::xs -> 
            match x with
                | PVar _ -> all_vars xs
                | _      -> false 
    in 
    let (pats, _) = row in
    all_vars pats
            
(* Partition matrix by first column constructor *)
let partition (Matrix mat) =
    let add_row part row = match row with
        | (p :: _, _) ->
            let tag = tag_of_pat p in
            let rows = match TagMap.find_opt tag part with
                | Some rs -> row :: rs
                | None    -> [row]
            in
            TagMap.add tag rows part
        | ([], _) -> failwith "row has no patterns"
    in
    let part = List.map
        (fun (tag, rows) -> (tag, Matrix rows))
        (TagMap.bindings (List.fold_left add_row TagMap.empty mat))
    in Partition part

(* Drop column 0 from matrix mat *)
let drop_col0 (Matrix mat) =
    let drop_acc new_mat row = match row with
        | (_ :: ps, e) -> (ps, e) :: new_mat
        | ([], _)      -> failwith "row has no patterns"
    in
    Matrix (List.fold_left drop_acc [] mat |> List.rev)

(* Get pattern from position 0,0 in matrix *)
let get_00 (Matrix mat) = 
    let p_list, _ = (List.nth mat 0) in List.nth p_list 0

(* Extract string from PVar to be used in a Let exp *)
let get_string var = match var with
    | PVar v         -> v
    | _              -> failwith "Improper use of get_string"

(* Expand scrutinee list with cons at the front *)
let expand_cons scruts = 
    match scruts with 
        | [] -> failwith "Trying to expand_cons empty scruts list"
        | scrut0::rest ->
            let x = Head scrut0 in
            let y = Tail scrut0 in
                Scruts (x::y::rest)

(* Expand scrutinee list with some constr at the front *)
let expand_constr scruts m =
    match scruts with
        | [] -> failwith "Trying to expand_constr empty scruts list"
        | scrut0 :: rest ->
        (* look at first pattern in the matrix to get arity *)
        let arity = match get_00 m with
            | PConstr (_, ps) -> List.length ps
            | _ -> failwith "expand_constr called but first pattern not a constructor"
        in
        let rec unpack_all i acc =
            if i = arity then List.rev acc
                else unpack_all (i + 1) (Unpack (scrut0, i) :: acc)
        in
        Scruts (unpack_all 0 [] @ rest)         

(* Expand the product constructor in the first column of the matrix *)
let expand_col0 (Matrix mat) = 
    let expand_row new_mat row = 
        let p_list, rhs = row in 
        let expanded_list = 
            match p_list with
                | []         -> failwith "Trying to expand empty p_list"
                | pat0::rest -> 
                    match pat0 with
                        | PCons (x, y)    -> x::y::rest
                        | PConstr (_, ps) -> ps @ rest
                        | _               -> failwith "Trying to expand non-product pattern"
        in
        (expanded_list, rhs) :: new_mat
    in
    Matrix (List.fold_left expand_row [] mat |> List.rev)

(* Generate a chain of bindings from a row that is all variables + rhs *)
let rec gen_bindings scruts row = match row with
    | ([], rhs)    -> rhs
    | (x::xs, rhs) -> 
        match scruts with
            | [] -> failwith "Scruts empty when trying to gen_bindings"
            | e::es -> Let (get_string x, e, gen_bindings es (xs, rhs))

(* Generate the tests that form the branches of the decision tree *)
let rec gen_tests (Scruts scruts) (Partition part) = match part with
    | []           -> Fail
    | (pt, m)::rst -> 
        match scruts with
            | []          -> failwith "Empty scruts in gen_tests"
            | scrut::rest ->
                match pt with
                    | TagVar -> Let (get_string (get_00 m), 
                                  scrut, 
                                    compile_match (Scruts rest) (drop_col0 m))

                    | TagInt n -> If (Eq (Int n, scrut), 
                                    compile_match (Scruts rest) (drop_col0 m), 
                                      gen_tests (Scruts scruts) (Partition rst))

                    | TagBool b -> If (Eq (Bool b, scrut), 
                                    compile_match (Scruts rest) (drop_col0 m), 
                                      gen_tests (Scruts scruts) (Partition rst))

                    | TagEmpty -> If (Eq (Empty, scrut), 
                                    compile_match (Scruts rest) (drop_col0 m), 
                                      gen_tests (Scruts scruts) (Partition rst))

                    | TagCons -> If (IsCons scrut, 
                                   compile_match (expand_cons scruts) (expand_col0 m), 
                                     gen_tests (Scruts scruts) (Partition rst))
                    
                    | TagConstr c -> If (IsConstr (scrut, c), 
                                       compile_match (expand_constr scruts m) (expand_col0 m), 
                                         gen_tests (Scruts scruts) (Partition rst))

(* pattern match compilation driver *)
and compile_match (Scruts scruts) (Matrix mat) = match mat with
    | []      -> Fail
    | row0::_ -> 
        if inspect_row row0 
            then gen_bindings scruts row0
            else gen_tests (Scruts scruts) (partition (Matrix mat))