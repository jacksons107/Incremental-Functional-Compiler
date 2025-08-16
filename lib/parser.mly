%{
open Ast
%}

%token <string> VAR
%token <int> INT 
%token <bool> BOOL
%token CONS
%token HEAD
%token TAIL
%token EMPTY
%token PLUS
%token IF
%token THEN
%token ELSE
%token LET
%token DEF
%token EQ
%token IN
%token LPAREN
%token RPAREN
%token COMMA
%token EOF

%left PLUS


%start <Ast.exp> prog

%%


prog:
    | e = exp; EOF {e}

exp:
    | LET; v = VAR; EQ; b = exp; IN; e = exp {Let (v, b, e)}
    | DEF; n = VAR; v = nonempty_list(VAR); EQ; e = exp; IN; r = exp {Def (n, v, e, r)}
    | IF; b = exp; THEN; e1 = exp; ELSE; e2 = exp {If (b, e1, e2)}
    | e = add_exp {e}

add_exp:
    | e1 = add_exp; PLUS; e2 = add_exp {Plus (e1, e2)}
    | e = app_exp {e}

app_exp:
    | CONS; LPAREN; e1 = exp; COMMA; e2 = exp; RPAREN {Cons (e1, e2)}
    | CONS; LPAREN; e = exp; RPAREN {Cons (e, Empty)}
    | HEAD; c = atom {Head c}
    | TAIL; c = atom {Tail c}
    | f = app_exp; arg = atom {App (f, arg)}
    | a = atom { a }

atom:
    | i = INT {Int i}
    | b = BOOL {Bool b}
    | v = VAR {Var v}
    | EMPTY {Empty}
    | LPAREN; e = exp; RPAREN {e}
