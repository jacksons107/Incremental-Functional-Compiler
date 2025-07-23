%{
open Ast
%}

%token <string> VAR
%token <int> INT 
%token <bool> BOOL
%token PLUS
%token IF
%token THEN
%token ELSE
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

exp:
    | LET; v = VAR; EQ; b = exp; IN; e = exp {Let (v, b, e)}
    | IF; b = exp; THEN; e1 = exp; ELSE; e2 = exp {If (b, e1, e2)}
    | e = add_exp {e}

add_exp:
    | e1 = add_exp; PLUS; e2 = add_exp {Plus (e1, e2)}
    | e = atom {e}

atom:
    | i = INT {Int i}
    | b = BOOL {Bool b}
    | v = VAR {Var v}
    | LPAREN; e = exp; RPAREN {e}
