options {
    language=Python;
}

class sl_parser extends Parser;

options {
    buildAST = true;
//    exportVocab = RenderManSL;
    k = 2;
}

tokens {
    FUNDEF;
    SHADERDEF;
    ARR;
    PROCCALL;
}

program : (function | shader)*;
function!: rt:return_type name:ID "(" (args:formal_args)? ")" body:body
    { ## = #([FUNDEF], #rt, #name, #args, #body)};
shader!: st:shader_type name:ID "(" (args:formal_args)? ")" body:body
    { ## = #([SHADERDEF], #st, #name, #args, #body)};

return_type:
    "void"
    | "float"
    | "point"
    | "vector"
    | "normal"
    | "color"
    | "matrix"
    | "string"
;

arg_type:
      "float"
    | "point"
    | "vector"
    | "normal"
    | "color"
    | "matrix"
    | "string"
;

shader_type:
      "displacement"
    | "volume"
    | "surface"
    | "light" 
    | "imager"
;

formal_args: formal_argx (";"! formal_argx)* (";"!)?;

formal_argx: ("output")? ("varying"|"uniform")? arg_type def_exprs;

def_exprs: def_expr ("," def_expr)*;

def_expr: ID ("="! expr)?;

body: "{"! (statement)* "}"!;

statement:
    expr ";"!
    | "return"^ expr ";"!
    | "break"^ (NUMBER)?  ";"!
    | "continue"^ (NUMBER)?  ";"!
    | "{" (statement) * "}"
    | "if" "("! expr ")"! statement
//    | "if" "("! expr ")"! statement "else" statement
    | "while" "("! expr ")"! statement
    | "for" "("! expr ";"! expr ";"! "expr )"! statement
;

expr2p: expr ("," expr)+;

var!:
    base:ID
	{ ## = #base }
    (
	"["!
	index:expr
	    { ## = #([ARR], #base, #index)}
	"]"!
    )?;

proccall!:
    procname:ID { ## = #([PROCCALL], #procname) }
    "("!
    (
	args:actual_args { ##.addChild(#args) }
    )?
    ")"!
;
actual_args: expr (","! expr)*;

expr:  expr1 (("="^|"+="^|"-="^|"*="^|"/="^) expr)?; // right-associative
expr1: expr2 ("?"^ expr2 ":"! expr1)?; // right-associative
expr2: expr3 ("||"^ expr3)*; // left-associative
expr3: expr4 ("&&"^ expr4)*; // left-associative
expr4: expr5 (("=="^|"!="^|">="^|"<="^|">"^|"<"^) expr5)?; //non-associative
expr5: expr6 (("+"^|"-"^) expr6) *; // left-associative
expr6: expr7 ("^"^       expr7) *; // left-associative
expr7: expr8 (("*"^|"/"^) expr8) *; // left-associative
expr8: expr9 ("."^       expr9) *; // left-associative
expr9:
      NUMBER
    | STR
    | var
    | proccall
    | "("! expr ")"!
//    | "(" expr2p ")"
    | "-"^ expr9
    | "!"^ expr9
    | typecast expr9
    | ("texture"^|"environment"^|"shadow"^) "("! actual_args ")"!
;

// TODO: Add (STR)?
typecast:
      "float"^
    | "point"^
    | "vector"^
    | "normal"^
    | "color"^
    | "string"^
    | "matrix"^
;

class sl_lexer extends Lexer;

options {
    k=2;
}

protected
INT : ('0'..'9')+;

NUMBER : INT ("." INT)? ;

STR : '"' (~('"'|'\\')|'\\'.)* '"';

ID : ('a'..'z' | 'A'..'Z' | '_') ('a'..'z' | 'A'..'Z' | '_' | '0'..'9') *;

WS  :
    (   ' '
    |   '\t'
    |   '\r'
    |   '\n' { $newline }
    )
    { $skip }  //ignore this token
;

// All interpunction goes here (UGLY)
INTERPUNCTION : (
    "+"|"-"|"*"|"/"|"."|"^"
    |"("|")"|"["|"]"|"{"|"}"|","|";"
    |"="|"+="|"-="|"*="|"/="
    |"=="|"!="
    |">="|"<="|">"|"<"
)
;
