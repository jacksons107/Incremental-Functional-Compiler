type j_instr =
    | PushInt of int
    | Return

let emit_instr instr = match instr with
    | PushInt n -> Printf.sprintf "\tmov w0, #%d" n
    | Return    -> Printf.sprintf "\tret"

let run_j_machine instrs = String.concat "\n" (List.map emit_instr instrs)