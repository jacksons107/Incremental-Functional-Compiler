open Elam
open Ast_to_elam

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

let rec pp_elam elam_exp = match elam_exp with
    | EVar x -> "Var " ^ x
    | EInt n -> string_of_int n
    | EBool b -> string_of_bool b
    | EEq -> "=="
    | EIsEmpty -> "IsEmpty"
    | EIsCons -> "IsCons"
    | EIsInt -> "IsInt"
    | EPlus -> "+"
    | EUnpack -> "Unpack"
    | EIsConstr -> "IsConstr"
    | EIf -> "If"
    | EY -> "Y"
    | EHead -> "HEAD"
    | ETail -> "TAIL"
    | ECons -> "CONS"
    | EConstr (c, n) -> "Constr (" ^ c ^ ", " ^ string_of_int n ^ ")"
    | EEmpty -> "[]"
    | EFail -> "Fail"
    | EApp (e1, e2) -> "(" ^ pp_elam e1 ^ " " ^ pp_elam e2 ^ ")"
    | ELam (v, b) -> "(lambda (" ^ v ^ ") (" ^ pp_elam b ^ "))"
    | ELet (v, b, e) -> "(let " ^ v ^ " = " ^ pp_elam b ^ " in " ^ pp_elam e ^ ")"

let exp = "type test = Test of int * cons * int in
            type result = Res of int * int * int * int in
            let t = Test (1, Cons (2, 3), 4) in
            match t with
                Test (w, (8, y), z)  -> 420
                | Test (w, (x, 3), z)  -> Res (z, 69, x, w)"

let () = print_endline (pp_elam (ast_to_elam (parse exp)))