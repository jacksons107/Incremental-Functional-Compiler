open Compiler

let pp_token fmt (t : Parser.token) =
  let s =
    match t with
    | Parser.VAR v -> Printf.sprintf "VAR %S" v
    | INT n -> Printf.sprintf "INT %d" n
    | BOOL b -> Printf.sprintf "BOOL %b" b
    | CONSTR c -> Printf.sprintf "CONSTR %S" c
    | TYPINT -> "TYPINT"
    | TYPBOOL -> "TYPBOOL"
    | TYPSTRING -> "TYPSTRING"
    | TYPLIST -> "TYPLIST"
    | SEMI -> "SEMI"
    | CONS -> "CONS"
    | HEAD -> "HEAD"
    | TAIL -> "TAIL"
    | EMPTY -> "EMPTY"
    | PLUS -> "PLUS"
    | STAR -> "STAR"
    | IF -> "IF"
    | THEN -> "THEN"
    | ELSE -> "ELSE"
    | LET -> "LET"
    | DEF -> "DEF"
    | DEFREC -> "DEFREC"
    | TYPE -> "TYPE"
    | OF -> "OF"
    | MATCH -> "MATCH"
    | WITH -> "WITH"
    | BAR -> "BAR"
    | ARROW -> "ARROW"
    | BIND -> "BIND"
    | EQ -> "EQ"
    | IN -> "IN"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACK -> "LBRACK"
    | RBRACK -> "RBRACK"
    | COMMA -> "COMMA"
    | EOF -> "EOF"
  in
  Format.pp_print_string fmt s

let token = Alcotest.testable pp_token ( = )

let tokenize s =
  let lexbuf = Lexing.from_string s in
  let rec loop acc =
    match Lexer.read lexbuf with
    | Parser.EOF -> List.rev (Parser.EOF :: acc)
    | t -> loop (t :: acc)
  in
  loop []

let check name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check (Alcotest.list token) name
        (expected @ [ Parser.EOF ])
        (tokenize input))

let suite =
  [
    check "keywords: if/then/else" "if then else" Parser.[ IF; THEN; ELSE ];
    check "keywords: let/def/defrec/type/of/match/with/head/tail"
      "let def defrec type of match with head tail"
      Parser.[ LET; DEF; DEFREC; TYPE; OF; MATCH; WITH; HEAD; TAIL ];
    check "operators: + * == = -> :: |" "+ * == = -> :: |"
      Parser.[ PLUS; STAR; EQ; BIND; ARROW; CONS; BAR ];
    check "punctuation: ( ) [ ] , ;" "( ) [ ] , ;"
      Parser.[ LPAREN; RPAREN; LBRACK; RBRACK; COMMA; SEMI ];
    check "positive int literal" "42" Parser.[ INT 42 ];
    check "negative int literal" "-7" Parser.[ INT (-7) ];
    check "bool literals" "True False" Parser.[ BOOL true; BOOL false ];
    check "var identifiers incl. underscore/apostrophe" "foo_bar x' _y"
      Parser.[ VAR "foo_bar"; VAR "x'"; VAR "_y" ];
    check "constr identifiers" "Foo Bar123"
      Parser.[ CONSTR "Foo"; CONSTR "Bar123" ];
    check "empty-list is a single EMPTY token, not LBRACK RBRACK" "[]"
      Parser.[ EMPTY ];
    check "composite: let binding statement" "let x = 5;"
      Parser.[ LET; VAR "x"; BIND; INT 5; SEMI ];
    check "composite: newlines don't affect tokens" "let x =\n  5;"
      Parser.[ LET; VAR "x"; BIND; INT 5; SEMI ];
    check "empty input yields just EOF" "" [];
  ]
