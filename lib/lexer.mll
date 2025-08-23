{
open Parser

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <-
    { pos with
      Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
      Lexing.pos_bol = pos.Lexing.pos_cnum;
    }

}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let var = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
let constr = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = 
    parse
    | white {read lexbuf}
    | "\n" {incr_linenum lexbuf; read lexbuf}
    | "+" {PLUS}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "True" {BOOL true}
    | "False" {BOOL false}
    | "(" {LPAREN}
    | ")" {RPAREN}
    | "," {COMMA}
    | "let" {LET}
    | "def" {DEF}
    | "defrec" {DEFREC}
    | "type" {TYPE}
    | "match" {MATCH}
    | "with" {WITH}
    | "|" {BAR}
    | "->" {ARROW}
    | "=" {BIND}
    | "==" {EQ}
    | "in" {IN}
    | "Cons" {CONS}
    | "head" {HEAD}
    | "tail" {TAIL}
    | "[]" {EMPTY}
    | "[" {LBRACK}
    | "]" {RBRACK}
    | var {VAR (Lexing.lexeme lexbuf)}
    | constr {CONSTR (Lexing.lexeme lexbuf)}
    | int {INT (int_of_string (Lexing.lexeme lexbuf))}
    | eof {EOF}