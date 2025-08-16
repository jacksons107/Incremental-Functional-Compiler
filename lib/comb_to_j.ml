open Comb
open J_machine

let rec comb_to_j exp = match exp with
    | CInt n         -> [INT n]
    | CBool b        -> [BOOL b]
    | CCons (e1, e2) -> comb_to_j e2 @ comb_to_j e1 @ [CONS]
    | CHead          -> [GLOBAL (1, HEAD)]
    | CTail          -> [GLOBAL (1, TAIL)]
    | CPlus          -> [GLOBAL (2, ADD)]
    | CIf            -> [GLOBAL (3, IF)]
    | CY             -> [GLOBAL (1, Y)]
    | I              -> [GLOBAL (1, I)]
    | K              -> [GLOBAL (2, K)]
    | S              -> [GLOBAL (3, S)]
    | CApp (e1, e2) -> comb_to_j e2 @ comb_to_j e1 @ [APP]
    | _             -> failwith "unimplemented"

