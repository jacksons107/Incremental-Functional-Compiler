type code_ptr =
    | ADD
    | EQ | ISCONS | ISCONSTR
    | IF
    | CONS | HEAD | TAIL
    | UNPACK
    | Y
    | I | K | S

type j_instr =
    | INT of int
    | BOOL of bool
    | EMPTY
    | FAIL
    | GLOBAL of int * code_ptr
    | CONSTR of int * string
    | APP

let builtin_fn name = match name with
    | ADD      -> "eval_add"
    | EQ       -> "eval_eq"
    | ISCONS   -> "eval_iscons"
    | ISCONSTR -> "eval_isconstr"
    | IF       -> "eval_if"
    | CONS     -> "eval_cons"
    | HEAD     -> "eval_head"
    | TAIL     -> "eval_tail"
    | UNPACK   -> "eval_unpack"
    | Y        -> "eval_Y"
    | I        -> "eval_I"
    | K        -> "eval_K"
    | S        -> "eval_S"

let builtin_name name = match name with
    | ADD      -> "\"ADD\""
    | EQ       -> "\"EQ\""
    | ISCONS   -> "\"ISCONS\""
    | ISCONSTR -> "\"ISCONSTR\""
    | IF       -> "\"IF\""
    | CONS     -> "\"CONS\""
    | HEAD     -> "\"HEAD\""
    | TAIL     -> "\"TAIL\""
    | UNPACK   -> "\"UNPACK\""
    | Y        -> "\"Y\""
    | I        -> "\"I\""
    | K        -> "\"K\""
    | S        -> "\"S\""

let emit_instr instr = match instr with
    | INT n            -> Printf.sprintf "stack_push(mk_int(%d));" n
    | BOOL b           -> Printf.sprintf "stack_push(mk_bool(%B));" b
    | EMPTY            -> "stack_push(mk_empty());"
    | FAIL             -> "stack_push(mk_fail());"
    | GLOBAL (n, name) -> Printf.sprintf "stack_push(mk_global(%d, %s, %s));" n (builtin_fn name) (builtin_name name)
    | CONSTR (n, name) -> Printf.sprintf "stack_push(mk_constr(%d, %s));" n ("\"" ^ name ^ "\"")
    | APP              -> "stack_push(mk_app(stack_pop(), stack_pop()));"

let build_graph instrs = String.concat "\n" (List.map emit_instr instrs)

let run_j_machine instrs = build_graph instrs
