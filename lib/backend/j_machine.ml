type code_ptr =
  | ADD
  | EQ
  | ISCONS
  | ISCONSTR
  | IF
  | CONS
  | HEAD
  | TAIL
  | UNPACK
  | Y
  | I
  | K
  | S

type j_instr =
  | INT of int
  | BOOL of bool
  | STRING of string
  | EMPTY
  | FAIL
  | GLOBAL of int * code_ptr
  | CONSTR of int * string
  | APP
  | ID of string

let pp_code_ptr fmt name =
  let s =
    match name with
    | ADD -> "ADD"
    | EQ -> "EQ"
    | ISCONS -> "ISCONS"
    | ISCONSTR -> "ISCONSTR"
    | IF -> "IF"
    | CONS -> "CONS"
    | HEAD -> "HEAD"
    | TAIL -> "TAIL"
    | UNPACK -> "UNPACK"
    | Y -> "Y"
    | I -> "I"
    | K -> "K"
    | S -> "S"
  in
  Format.fprintf fmt "%s" s

let pp_instr fmt instr =
  match instr with
  | INT n -> Format.fprintf fmt "INT %d" n
  | BOOL b -> Format.fprintf fmt "BOOL %b" b
  | STRING s -> Format.fprintf fmt "STRING %S" s
  | EMPTY -> Format.fprintf fmt "EMPTY"
  | FAIL -> Format.fprintf fmt "FAIL"
  | GLOBAL (n, name) -> Format.fprintf fmt "GLOBAL(%d, %a)" n pp_code_ptr name
  | CONSTR (n, name) -> Format.fprintf fmt "CONSTR(%d, %s)" n name
  | APP -> Format.fprintf fmt "APP"
  | ID name -> Format.fprintf fmt "ID %s" name

let builtin_fn name =
  match name with
  | ADD -> "eval_add"
  | EQ -> "eval_eq"
  | ISCONS -> "eval_iscons"
  | ISCONSTR -> "eval_isconstr"
  | IF -> "eval_if"
  | CONS -> "eval_cons"
  | HEAD -> "eval_head"
  | TAIL -> "eval_tail"
  | UNPACK -> "eval_unpack"
  | Y -> "eval_Y"
  | I -> "eval_I"
  | K -> "eval_K"
  | S -> "eval_S"

let builtin_name name =
  match name with
  | ADD -> "\"ADD\""
  | EQ -> "\"EQ\""
  | ISCONS -> "\"ISCONS\""
  | ISCONSTR -> "\"ISCONSTR\""
  | IF -> "\"IF\""
  | CONS -> "\"CONS\""
  | HEAD -> "\"HEAD\""
  | TAIL -> "\"TAIL\""
  | UNPACK -> "\"UNPACK\""
  | Y -> "\"Y\""
  | I -> "\"I\""
  | K -> "\"K\""
  | S -> "\"S\""

(* [resolve] looks up a definition's assembly-time globals[] index by name
   -- built by the assembly step (Driver.compile_to_c) from the processing
   order of whatever definitions the program's entry point transitively
   needs. Nothing is rendered until every ID in the program is already
   resolvable, so resolve is total over every name emit_instr will ever
   see here -- there's no placeholder/unresolved case to handle. *)
let emit_instr resolve instr =
  match instr with
  | INT n -> Printf.sprintf "stack_push(mk_int(%d));" n
  | BOOL b -> Printf.sprintf "stack_push(mk_bool(%B));" b
  | STRING s -> Printf.sprintf "stack_push(mk_string(%s));" ("\"" ^ s ^ "\"")
  | EMPTY -> "stack_push(mk_empty());"
  | FAIL -> "stack_push(mk_fail());"
  | GLOBAL (n, name) ->
      Printf.sprintf "stack_push(mk_global(%d, %s, %s));" n (builtin_fn name)
        (builtin_name name)
  | CONSTR (n, name) ->
      Printf.sprintf "stack_push(mk_constr(%d, %s));" n ("\"" ^ name ^ "\"")
  | APP -> "stack_push(mk_app(stack_pop(), stack_pop()));"
  | ID name -> Printf.sprintf "stack_push(globals_get(%d));" (resolve name)

let build_graph resolve instrs =
  String.concat "\n" (List.map (emit_instr resolve) instrs)
