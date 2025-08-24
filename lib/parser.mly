%{
open Ast
%}

%token <string> VAR
%token <int> INT 
%token <bool> BOOL
%token <string> CONSTR
%token CONS
%token HEAD
%token TAIL
%token EMPTY
%token PLUS
%token STAR
%token IF
%token THEN
%token ELSE
%token LET
%token DEF
%token DEFREC
%token TYPE
%token OF
%token MATCH
%token WITH
%token BAR
%token ARROW
%token BIND
%token EQ
%token IN
%token LPAREN
%token RPAREN
%token LBRACK
%token RBRACK
%token COMMA
%token EOF

%left PLUS
%left EQ


%start <Ast.exp> prog

%%


prog:
    | e = exp; EOF {e}

exp:
    | LET; v = VAR; BIND; b = exp; IN; e = exp {Let (v, b, e)}
    | DEF; n = VAR; v = list(VAR); BIND; e = exp; IN; r = exp {Def (n, v, e, r)}
    | DEFREC; n = VAR; v = list(VAR); BIND; e = exp; IN; r = exp {Defrec (n, v, e, r)}
    | TYPE; n = VAR; BIND; c = CONSTR; OF; a = separated_list(STAR, VAR); IN; r = exp {Type (n, c, a, r)}
    | MATCH; scrut = exp; WITH; cases = match_cases {Match (scrut, cases)}
    | IF; b = exp; THEN; e1 = exp; ELSE; e2 = exp {If (b, e1, e2)}
    | e = bool_exp {e}

match_cases:
    | c = match_case {[c]}
    | c = match_case; BAR; cs = match_cases {c::cs}


match_case:
    | p = pat; ARROW; e = exp {(p, e)}

pat:
    | i = INT {PInt i}
    | b = BOOL {PBool b}
    | v = VAR {PVar v}
    | LPAREN; e1 = pat; COMMA; e2 = pat; RPAREN {PCons (e1, e2)}
    | c = CONSTR; LPAREN; a = separated_list(COMMA, pat); RPAREN {PConstr (c, a)}
    | EMPTY {PEmpty}

bool_exp:
    | e1 = bool_exp; EQ; e2 = bool_exp {Eq (e1, e2)}
    | e = math_exp {e}

math_exp:
    | e1 = math_exp; PLUS; e2 = math_exp {Plus (e1, e2)}
    | e = app_exp {e}

// TODO -- change outer CONS to infix ::
app_exp:
    | CONS; LPAREN; e1 = exp; COMMA; e2 = exp; RPAREN {Cons (e1, e2)}
    | CONS; LPAREN; e = exp; RPAREN {Cons (e, Empty)}   // get rid of this?
    | c = CONSTR; LPAREN; a = separated_list(COMMA, exp); RPAREN {Pack (c, a)}
    | HEAD; c = atom {Head c}
    | TAIL; c = atom {Tail c}
    | f = app_exp; arg = atom {App (f, arg)}
    | a = atom { a }

atom:
    | i = INT {Int i}
    | b = BOOL {Bool b}
    | v = VAR {Var v}
    | EMPTY {Empty}
    | LBRACK; d = separated_list(COMMA, atom); RBRACK {List d}
    | LPAREN; e = exp; RPAREN {e}
