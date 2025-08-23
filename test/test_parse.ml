open Ast

let parse s =
    let lexbuf = Lexing.from_string s in
    let ast = Parser.prog Lexer.read lexbuf in
    ast

let rec pp_pattern pat =
  match pat with
  | PVar x -> "PVar " ^ x
  | PInt n -> "PInt " ^ string_of_int n
  | PBool b -> "PBool " ^ string_of_bool b
  | PEmpty -> "PEmpty"
  | PCons (p1, p2) ->
      "PCons (" ^ pp_pattern p1 ^ ", " ^ pp_pattern p2 ^ ")"

let rec pp_list list = match list with
    | [] -> ""
    | (x::xs) -> x ^ ", " ^ pp_list xs

let rec pp_explist list = match list with
    | [] -> ""
    | (x::xs) -> pp_ast x ^ ", " ^ pp_explist xs
and pp_ast exp = match exp with
    | Var x   -> "Var " ^ x
    | Int n   -> string_of_int n
    | Bool b  -> string_of_bool b
    | Eq (e1, e2) -> "Eq (" ^ pp_ast e1 ^ ", " ^ pp_ast e2 ^ ")"
    | IsEmpty e -> "IsEmpty (" ^ pp_ast e ^ ")"
    | IsCons e -> "IsCons (" ^ pp_ast e ^ ")"
    | IsInt n -> "IsInt (" ^ pp_ast n ^ ")"
    | Empty   -> "[]"
    | Type (n, c, a, r) -> "Type (" ^ n ^ ", " ^ c ^ ", " ^ "(" ^ pp_list a ^ "), " ^ pp_ast r ^ ")"
    | Fail    -> "Fail"
    | Cons (e1, e2) -> "CONS (" ^ pp_ast e1 ^ ", " ^ pp_ast e2 ^ ")"
    | List l -> "[" ^ pp_explist l ^ "]"
    | Head c -> "HEAD (" ^ pp_ast c ^ ")"
    | Tail c -> "TAIL (" ^ pp_ast c ^ ")"
    | App (e1, e2) -> "App (" ^ pp_ast e1 ^ ", " ^ pp_ast e2 ^ ")"
    | Plus (e1, e2) -> "Plus (" ^ pp_ast e1 ^ ", " ^ pp_ast e2 ^ ")"
    | If (b, e1, e2) -> "If (" ^ pp_ast b ^ ", " ^ pp_ast e1 ^ ", " ^ pp_ast e2 ^ ")"
    | Let (v, b, e) -> "Let (" ^ v ^ ", " ^ pp_ast b ^ ", " ^ pp_ast e ^ ")"
    | Def (n, v, e, r) -> "Def (" ^ n ^ ", " ^ (String.concat " " v) ^ ", " ^ pp_ast e ^ ", " ^ pp_ast r ^ ")"
    | Defrec (n, v, e, r) -> "Defrec (" ^ n ^ ", " ^ (String.concat " " v) ^ ", " ^ pp_ast e ^ ", " ^ pp_ast r ^ ")"
    | Match (scrut, clauses) ->
        let pp_clause (p, e) =
        "(" ^ pp_pattern p ^ " -> " ^ pp_ast e ^ ")"
        in
        let clauses_str = String.concat "; " (List.map pp_clause clauses) in
        "Match (" ^ pp_ast scrut ^ ", [" ^ clauses_str ^ "])"




let exp1 = "let x = 3 in (let y = 4 in x + y)"
let exp2 = "def add4 x = x + 4 in add4 3"
let exp3 = "def not b = if b then False else True in not True"
let exp4 = "def add x y = x + y in add 69 420"
let exp5 = "def add x y = x + y in 
            let x = 69 in
            let y = 420 in
            let pair = CONS (x, y) in
            HEAD pair"
let exp6 = "[]"
let exp7 = "defrec sum l h = if l == h then l else l + (sum (l + 1) h) in
            sum 0 5"
let exp8 = "def inc x = x + 1 in inc 2"
let exp9 = "def sum_pair pair = match pair with
                [] -> False
                | (x, y) -> x + y
            in
            sum_pair (CONS (2, 3))"
let exp10 = "let x = [1, 2, 3] in x"
let exp11 = "def add a b = a + b in
             defrec sum_list fun l = match l with
                  [] -> 0
                | (x, xs) -> fun x (sum_list fun xs)
            in
            sum_list add [1, 2, 3 ,4]"
let exp12 = "def sum_np nest_pair = 
                match nest_pair with ((x, y), z) -> x + y + z in
            sum_np (CONS (CONS (1, 2), 3))"
let exp13 = "def sum_np nest_pair = 
                match nest_pair with ((x, y), z) -> x + y + z in
            sum_np [[1, 2], 3]"
let exp14 = "def lit_test pair = match pair with
                (0, x) -> x
                | (1, y) -> y + 1 in
            lit_test (CONS (1, 3))"

let exp15 = "type test = TEST int int int in 69"

let () = 
    print_endline (pp_ast (parse exp1));
    print_endline (pp_ast (parse exp2));
    print_endline (pp_ast (parse exp3));
    print_endline (pp_ast (parse exp4));
    print_endline (pp_ast (parse exp5));
    print_endline (pp_ast (parse exp6));
    print_endline (pp_ast (parse exp7));
    print_endline (pp_ast (parse exp8));
    print_endline (pp_ast (parse exp9));
    print_endline (pp_ast (parse exp10));
    print_endline (pp_ast (parse exp11));
    print_endline (pp_ast (parse exp12));
    print_endline (pp_ast (parse exp13));
    print_endline (pp_ast (parse exp14));
    print_endline (pp_ast (parse exp15))



