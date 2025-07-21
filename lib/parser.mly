%{
open Ast
%}

%token <string> VAR
%token <int> INT 
%token PLUS
%token LET
%token EQ
%token IN
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
    | v = VAR {Var v}
    | e1 = exp; PLUS; e2 = exp {Plus (e1, e2)}
    | LET; v = VAR; EQ; b = exp; IN; e = exp {Let (v, b, e)}
    | LPAREN; e = exp; RPAREN {e}
    ;



