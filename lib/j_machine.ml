type code_ptr =
    | ADD
    | I | K | S

type j_instr =
    | INT of int
    | GLOBAL of int * code_ptr
    | APP

let builtin_fn name = match name with
    | ADD -> "eval_add"
    | I   -> "eval_I"
    | K   -> "eval_K"
    | S   -> "eval_S"

let builtin_name name = match name with
    | ADD -> "\"ADD\""
    | I   -> "\"I\""
    | K   -> "\"K\""
    | S   -> "\"S\""

let emit_instr instr = match instr with
    | INT n            -> Printf.sprintf "stack_push(mk_int(%d));" n
    | GLOBAL (n, name) -> Printf.sprintf "stack_push(mk_global(%d, %s, %s));" n (builtin_fn name) (builtin_name name)
    | APP              -> "stack_push(mk_app(stack_pop(), stack_pop()));"

let build_graph instrs = String.concat "\n" (List.map emit_instr instrs)

let run_j_machine instrs = build_graph instrs
