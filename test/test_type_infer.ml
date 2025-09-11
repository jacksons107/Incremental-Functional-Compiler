open Type_infer
open Ast_to_elam
open Def_to_exp

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

let rec pp_typ ty = match ty with
    | TInt -> "Int"
    | TBool -> "Bool"
    | TLam (t1, t2) -> pp_typ t1 ^ " -> " ^ pp_typ t2
    | TList t -> pp_typ (prune t) ^ " List"
    | TVar _ -> "Var?"
    (* | TVar _ -> pp_typ (prune ty) *)

let exp = "tail [1, 2, 3]"

let () = print_endline (pp_typ (infer (ast_to_elam (def_to_exp (parse exp))) empty_env))
