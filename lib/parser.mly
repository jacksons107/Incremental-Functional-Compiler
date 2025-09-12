%{
open Ast
%}

%token <string> VAR
%token <int> INT 
%token <bool> BOOL
%token <string> CONSTR
%token SEMI
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


%start <Ast.prog> prog

%%


prog:
    | ds = list(def); e = exp; EOF {Prog (ds, e)}

def:
    | LET; v = VAR; BIND; b = exp; SEMI {DLet (v, b)}
    | DEF; n = VAR; v = list(VAR); BIND; e = exp; SEMI {DDef (n, v, e)}
    | DEFREC; n = VAR; v = list(VAR); BIND; e = exp; SEMI {DDefrec (n, v, e)}
    // | TYPE; n = VAR; BIND; c = CONSTR; OF; a = separated_list(STAR, VAR); SEMI {DType (n, c, a)}
    | TYPE; n = VAR; BIND; cs = separated_nonempty_list(BAR, constr_def); SEMI {DType (n, cs)}


constr_def:
    | c = CONSTR; OF; a = separated_list(STAR, VAR) {(c, a)}
    | c = CONSTR {(c, [])}


local_def:
    | LET; v = VAR; BIND; b = nonmatch_exp; IN; e = nonmatch_exp {Let (v, b, e)}
    | DEF; n = VAR; v = list(VAR); BIND; e = nonmatch_exp; IN; r = nonmatch_exp {Def (n, v, e, r)}
    | DEFREC; n = VAR; v = list(VAR); BIND; e = nonmatch_exp; IN; r = nonmatch_exp {Defrec (n, v, e, r)}

exp:
    | n = nonmatch_exp {n}
    | m = match_exp {m}

nonmatch_exp:
    | LPAREN; m = match_exp; RPAREN {m}
    | IF; b = nonmatch_exp; THEN; e1 = nonmatch_exp; ELSE; e2 = nonmatch_exp {If (b, e1, e2)}
    | LPAREN; l = local_def; RPAREN {l}
    | e = bool_exp {e}

match_exp:
    | MATCH; scrut = nonmatch_exp; WITH; cases = match_cases {Match ([scrut], cases)}

match_cases:
    | BAR; cs = separated_nonempty_list(BAR, match_case) {cs}

match_case:
    | p = pat; ARROW; e = nonmatch_exp {([p], e)}

pat:
    | i = INT {PInt i}
    | b = BOOL {PBool b}
    | v = VAR {PVar v}
    | LPAREN; e1 = pat; COMMA; e2 = pat; RPAREN {PCons (e1, PCons (e2, PEmpty))}
    | c = CONSTR; LPAREN; a = separated_list(COMMA, pat); RPAREN {PConstr (c, a)}
    | c = CONSTR {PConstr (c, [])}
    | EMPTY {PEmpty}

bool_exp:
    | e1 = math_exp; EQ; e2 = math_exp {Eq (e1, e2)}
    | e = math_exp {e}

math_exp:
    | e1 = math_exp; PLUS; e2 = math_exp {Plus (e1, e2)}
    | e = app_exp {e}

// TODO -- change outer CONS to infix ::
app_exp:
    | CONS; LPAREN; e1 = nonmatch_exp; COMMA; e2 = nonmatch_exp; RPAREN {Cons (e1, e2)}
    | CONS; LPAREN; e = nonmatch_exp; RPAREN {Cons (e, Empty)}   // get rid of this?
    | c = CONSTR; LPAREN; a = separated_list(COMMA, nonmatch_exp); RPAREN {Pack (c, a)}
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
    | LPAREN; e = nonmatch_exp; RPAREN {e}
