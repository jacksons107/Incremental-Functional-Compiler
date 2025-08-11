{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let var = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

rule read = 
    parse
    | white {read lexbuf}
    | "+" {PLUS}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "True" {BOOL true}
    | "False" {BOOL false}
    | "(" { LPAREN }
    | ")" { RPAREN }
    | "let" {LET}
    | "def" {DEF}
    | "=" {EQ}
    | "in" {IN}
    | var {VAR (Lexing.lexeme lexbuf)}
    | int {INT (int_of_string (Lexing.lexeme lexbuf))}
    | eof {EOF}