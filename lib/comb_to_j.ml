open Comb
open J_machine

let rec comb_to_j exp = match exp with
    | CInt n        -> [INT n]
    | CPlus         -> [GLOBAL (2, ADD)]
    | CApp (e1, e2) -> comb_to_j e2 @ comb_to_j e1 @ [APP]
    | _             -> failwith "unimplemented"

