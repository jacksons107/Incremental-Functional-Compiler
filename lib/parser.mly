%{
open Ast
%}

%token <int> INT 
%token PLUS
%token LPAREN
%token RPAREN
%token EOF

%left PLUS

%start <Ast.exp> prog

%%

prog:
    | e = exp; EOF {e}
    ;

exp:
    | i = INT {Int i}
    | e1 = exp; PLUS; e2 = exp {Plus (e1, e2)}
    | LPAREN; e = exp; RPAREN {e}
    ;



