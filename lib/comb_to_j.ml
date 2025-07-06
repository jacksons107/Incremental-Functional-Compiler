open Comb
open J_machine

let comb_to_j exp = match exp with
    | CInt n -> [PushInt n; Return]
    | _      -> failwith "unimplimented"