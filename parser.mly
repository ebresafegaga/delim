%token <int> INT
// %token <float> FLOAT
%token <string> ID
%token <string> STRING
%token TRUE 
%token FALSE 

%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK
%token COMMA
%token SEMICOLON
%token EQUALS
%token DEFINE 
%token FUN

%token PLUS MINUS TIMES DIV EQUALEQUAL GT LT

%token EOF

%start toplevel

%type <Syntax.toplevel list> toplevel
%type <Syntax.toplevel> top
%type <Syntax.toplevel> define
%type <Syntax.expr> expression

%type <Syntax.name list> param_list
%type <Syntax.name> param

%%

toplevel: tops = list(top); EOF { tops }

top: 
    | d = define { d }
    | e = expression { Syntax.TopExpr e }

define:     
    | DEFINE; name = ID; value = expression 
        { Syntax.TopExpr (name, value) }
    | DEFINE; name = ID; params = param_list; LBRACE; body = expression; RBRACE 
        { let lambda = Syntax.EFun (params, body) in 
          Syntax.TopExpr (name, lambda) }

expression: 
    | name = ID { Syntax.EVar name }
    | value = INT {  Syntax.EInt value }
    | value = STRING { Syntax.EString value }
    | TRUE { Syntax.EBool true }
    | FALSE { Syntax.EBool false }
    | IF; pred = expression; THEN tbranch = expression; ELSE fbranch = expression
        { Syntax.EIf (pred, tbranch, fbranch) }
    | LET; name = ID; EQUALS; value = expression; SEMICOLON; body = expression
        { Syntax.ELet (name, value, body) } 
    | FUN; params = param_list; body = expression
        { Syntax.EFun (params, body) }
    | rator = expression; LPAREN; rands = expr_list; RPAREN; 
        { Syntax.EApply (rator, rands) }
    | left = expression; op = operator; right = expression; 
        { Syntax.EApply (op, [left; right]) }
    | LBRACK; exprs = expr_list; RBRACK { Syntax.EList exprs }

%inline operator: 
    | TIMES { "*" } 
    | DIV { "/" }
    | PLUS { "+" }
    | MINUS { "-" }
    | GT { ">" }
    | LT { "<" }


// helpers 

param: id = ID; { id }

param_list: LPAREN; params=separated_list(COMMA,param); RPAREN { params }

expr_list: params=separated_list(COMMA,expression) { params }
