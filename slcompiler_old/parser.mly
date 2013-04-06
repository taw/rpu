%{
    open Ast
%}
%token<string> ID STR
%token<int> INT
%token<float> FLOAT
%token OPAREN CPAREN OCURLY CCURLY OSQUARE CSQUARE SEMI COMMA
%token K_EXTERN K_OUTPUT K_VARYING K_UNIFORM
%token K_LIGHT K_SURFACE K_VOLUME K_DISPLACEMENT K_IMAGER
%token K_IF K_ELSE K_WHILE K_FOR K_SOLAR K_ILLUMINATE K_ILLUMINANCE 
%token K_FLOAT K_STRING K_COLOR K_POINT K_VECTOR K_NORMAL K_MATRIX K_VOID
%token K_BREAK K_CONTINUE K_RETURN
%token K_TEXTURE K_ENVIRONMENT K_SHADOW
%token MINUS PLUS TIMES DIV QUEST COLON DOT CIRC
%token GT GE LT LE EQEQ NE AND OR
%token EQ PLUSEQ MINUSEQ TIMESEQ DIVEQ EXCL
%token EOF

%type <Ast.definition list> shaderfile
%type <Ast.asgexp> assignexpression
%type <Ast.proccall> procedurecall
%type <Ast.statement> statement
%type <Ast.expr> expression
%start shaderfile

%right EQ PLUSEQ MINUSEQ TIMESEQ DIVEQ
%right QUEST COLON

%left OR
%left AND

%left EQEQ NE
%left GT GE LT LE

%left PLUS MINUS
%left CIRC
%left TIMES DIV
%left DOT

%nonassoc UMINUS EXCL 
%nonassoc TYPECAST

%%
shaderfile:
| EOF                   { [] }
| definition shaderfile	{ $1::$2 }

definition:
| shader_definition	{ $1 }
| function_definition	{ $1 }

shader_definition:
shader_type ID OPAREN formals CPAREN body { `SHADER($1, $2, $4, $6) }

function_definition:
type_opt ID OPAREN formals CPAREN body { `FUNCTION($1, $2, $4, $6) }

body:
| OCURLY statements CCURLY { $2 }

type_opt:
| /* */  { `VOID }
| K_VOID { `VOID }
| typex  { ($1 :> Ast.fun_typespec) }

shader_type:
| K_LIGHT        { `LIGHT }
| K_SURFACE      { `SURFACE }
| K_VOLUME       { `VOLUME }
| K_DISPLACEMENT { `DISPLACEMENT }
| K_IMAGER       { `IMAGER }

formals:
| /* */  { [] }
| formal_variable_definitions { $1 }
| formal_variable_definitions SEMI formals { $1 @ $3 }

formal_variable_definitions:
outputspec_opt typespec def_expressions { List.map (fun (id,defval) -> `FORMAL($1,$2,id,defval)) $3 }

/* variables - RULE NOT USED */

variable_definitions:
externspec_opt typespec def_expressions { `BLOCK(List.map (fun (id,defval) -> `VARDEF($1,$2,id,defval)) $3) }

typespec:
detail_opt typex { (* Completely ignore uniform/varying here *) $2 }

def_expressions:
| def_expression { [$1] }
| def_expression COMMA def_expressions { $1::$3 }

def_expression:
ID def_init_opt { ($1, $2) }

def_init_opt:
| /* */         { None }
| EQ expression { Some $2 }

typex:
| K_FLOAT  { `FLOAT }
| K_STRING { `STRING }
| K_COLOR  { `COLOR }
| K_POINT  { `VEC(`POINT) }
| K_VECTOR { `VEC(`VECTOR) }
| K_NORMAL { `VEC(`NORMAL) }
| K_MATRIX { `MATRIX }

detail_opt:
| /* */     { (* `DEFAULT *) () }
| K_VARYING { (* `VARYING *) () }
| K_UNIFORM { (* `UNIFORM *) () }

outputspec_opt:
| /* */    { false }
| K_OUTPUT { true }

externspec_opt:
| /* */    { false }
| K_EXTERN { true }

statements:
| /* */                { [] }
| statement statements { $1::$2 }

statement:
| statement_simple { $1 }
| K_IF relation statement   { `IFELSE($2,[$3],[]) }
| K_IF relation statement_nocatchelse K_ELSE statement { `IFELSE($2,[$3],[$5]) }
| K_WHILE relation statement                                             { `WHILE($2,[$3]) }
| K_FOR OPAREN expression SEMI relation SEMI expression CPAREN statement { `FOR($3,$5,$7,[$9]) }
| K_SOLAR OPAREN expressionlist_opt CPAREN statement                     { `SOLAR($3,[$5]) }
| K_ILLUMINATE OPAREN expressionlist_opt CPAREN statement                { `ILLUMINATE($3,[$5]) }
| K_ILLUMINANCE OPAREN expressionlist_opt CPAREN statement               { `ILLUMINANCE($3,[$5]) }

statement_nocatchelse:
| statement_simple { $1 }
| K_IF relation statement_nocatchelse K_ELSE statement_nocatchelse { `IFELSE($2,[$3],[$5]) }
| K_WHILE relation statement_nocatchelse                                             { `WHILE($2,[$3]) }
| K_FOR OPAREN expression SEMI relation SEMI expression CPAREN statement_nocatchelse { `FOR($3,$5,$7,[$9]) }
| K_SOLAR OPAREN expressionlist_opt CPAREN statement_nocatchelse                     { `SOLAR($3,[$5]) }
| K_ILLUMINATE OPAREN expressionlist_opt CPAREN statement_nocatchelse                { `ILLUMINATE($3,[$5]) }
| K_ILLUMINANCE OPAREN expressionlist_opt CPAREN statement_nocatchelse               { `ILLUMINANCE($3,[$5]) }

statement_simple:
| loop_modstmt SEMI         { $1 }
| variable_definitions SEMI { $1 }
| assignexpression SEMI     { ($1 :> Ast.statement) }
| procedurecall SEMI        { ($1 :> Ast.statement) }
| K_RETURN expression SEMI  { `RETURN($2) }
| body                      { `BLOCK($1) }

/*
statement:
| loop_modstmt SEMI         { $1 }
| variable_definitions SEMI { $1 }
| assignexpression SEMI     { ($1 :> Ast.statement) }
| procedurecall SEMI        { ($1 :> Ast.statement) }
| K_RETURN expression SEMI  { `RETURN($2) }
| body                      { `BLOCK($1) }
| K_IF relation statement   { `IFELSE($2,[$3],[]) }
| K_IF relation statement K_ELSE statement { `IFELSE($2,[$3],[$5]) }
| K_WHILE relation statement                                             { `WHILE($2,[$3]) }
| K_FOR OPAREN expression SEMI relation SEMI expression CPAREN statement { `FOR($3,$5,$7,[$9]) }
| K_SOLAR OPAREN expressionlist_opt CPAREN statement                     { `SOLAR($3,[$5]) }
| K_ILLUMINATE OPAREN expressionlist_opt CPAREN statement                { `ILLUMINATE($3,[$5]) }
| K_ILLUMINANCE OPAREN expressionlist_opt CPAREN statement               { `ILLUMINANCE($3,[$5]) }
*/

loop_modstmt:
| K_BREAK integer_opt_default_1    { `BREAK($2) }
| K_CONTINUE integer_opt_default_1 { `CONTINUE($2) }

integer_opt_default_1:
| /* */ { 1 }
| INT   { $1 }

expressionlist_opt:
| /* */          { [] }
| expressionlist { $1 }

expressionlist:
| expression                      { [$1] }
| expression COMMA expressionlist { $1::$3 }

expressionlist2p:
| expression COMMA expressionlist { $1::$3 }

expression:
| expression DOT expression                  { `DOTPROD($1,$3) }
| expression DIV expression                  { `DIV($1,$3) }
| expression TIMES expression                { `TIMES($1,$3) }
| expression CIRC expression                 { `CROSSPROD($1,$3) }
| expression PLUS expression                 { `PLUS($1,$3) }
| expression MINUS expression                { `MINUS($1,$3) }
| MINUS expression %prec UMINUS              { `UMINUS($2) }
| relation QUEST expression COLON expression { `MUX($1,$3,$5) }
| K_FLOAT expression %prec TYPECAST               { `CAST( `FLOAT,$2) }
| K_STRING expression %prec TYPECAST              { `CAST( `STRING,$2) }
| K_COLOR spacetype_opt expression %prec TYPECAST { `CAST( `COLOR($2),$3) }
| K_POINT spacetype_opt expression %prec TYPECAST { `CAST( `POINT($2),$3) }
| K_VECTOR spacetype_opt expression %prec TYPECAST { `CAST( `VECTOR($2),$3) }
| K_NORMAL spacetype_opt expression %prec TYPECAST { `CAST( `NORMAL($2),$3) }
| K_MATRIX spacetype_opt expression %prec TYPECAST { `CAST( `MATRIX($2),$3) }
| INT                            { `FLOAT(float_of_int $1) }
| FLOAT                          { `FLOAT($1) }
| STR                            { `STRING($1) }
| texture                        { $1 }
| ID                             { `ID($1) }
| ID OSQUARE expression CSQUARE  { `ARR($1,$3) }
| procedurecall                  { ($1 :> Ast.expr) }
| assignexpression               { ($1 :> Ast.expr) }
| OPAREN expressionlist2p CPAREN   { `TUPLE($2) }
| OPAREN expression CPAREN      { $2 }

spacetype_opt:
| /* */ { None }
| STR   { Some $1}


relation:
| OPAREN relation CPAREN     { $2 }
| expression GT expression   { `GT($1,$3) }
| expression GE expression   { `GE($1,$3) }
| expression LT expression   { `LT($1,$3) }
| expression LE expression   { `LE($1,$3) }
| expression EQEQ expression { `EQEQ($1,$3) }
| expression NE expression   { `NE($1,$3) }
| relation AND relation      { `AND($1,$3) }
| relation OR relation       { `OR($1,$3) }
| EXCL relation              { `NEG($2) }

asgtarget:
| ID                            { `SCALAR($1) }
| ID OSQUARE expression CSQUARE { `ARRAYELT($1,$3) }

assignexpression:
| asgtarget EQ      expression { `ASG($1, `EQ, $3) }
| asgtarget PLUSEQ  expression { `ASG($1, `PLUSEQ, $3) }
| asgtarget MINUSEQ expression { `ASG($1, `MINUSEQ, $3) }
| asgtarget TIMESEQ expression { `ASG($1, `TIMESEQ, $3) }
| asgtarget DIVEQ   expression { `ASG($1, `DIVEQ, $3) }

procedurecall:
ID OPAREN proc_arguments_opt CPAREN { `PROCCALL($1,$3) }

proc_arguments_opt:
| /* */          { [] }
| proc_arguments { $1 }

proc_arguments:
| expression                      { [$1] }
| expression COMMA proc_arguments { $1::$3 }

texture:
texture_type OPAREN texture_filename channel_opt texture_arguments_opt CPAREN { `TEXTURE($1,$3,$4,$5) }

texture_type:
| K_TEXTURE     { `TEXTURE }
| K_ENVIRONMENT { `ENVIRONMENT }
| K_SHADOW      { `SHADOW }

texture_filename:
expression { $1 }

channel_opt:
| /* */ { None }
| OSQUARE expression CSQUARE { Some $2 }

texture_arguments_opt:
| /* */                                  { [] }
| COMMA expression texture_arguments_opt { $2::$3 }
