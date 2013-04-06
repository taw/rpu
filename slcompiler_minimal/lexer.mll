{
    open Parser
}
let space = [' ' '\t' '\n' '\r']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alphanum = ['a'-'z' 'A'-'Z' '_' '0'-'9']
let intrx = ['0'-'9'] +
let fltrx = ['0'-'9']* '.' intrx
let idrx  = alpha alphanum*
let any = ['\000'-'\255']
let strrx = "\"" ([^'"'] | "\\" any)* "\""
let cxx_comment = "//" [^'\n']* "\n"
let c_comment = "/*" ([^'*']|"*"[^'/'])* "*/"
let ignored = space + | c_comment | cxx_comment

rule token = parse
| ignored   { token lexbuf }
| "extern"  { K_EXTERN }
| "output"  { K_OUTPUT }
| "varying" { K_VARYING }
| "uniform" { K_UNIFORM }
| "light"   { K_LIGHT }
| "surface" { K_SURFACE }
| "volume"  { K_VOLUME }
| "displacement" { K_DISPLACEMENT }
| "imager"  { K_IMAGER }
| "if"      { K_IF }
| "else"    { K_ELSE }
| "while"   { K_WHILE }
| "for"     { K_FOR }
| "solar"   { K_SOLAR }
| "illuminate" { K_ILLUMINATE }
| "illuminance" { K_ILLUMINANCE }
| "float"   { K_FLOAT }
| "string"  { K_STRING }
| "color"   { K_COLOR }
| "point"   { K_POINT }
| "vector"  { K_VECTOR }
| "normal"  { K_NORMAL }
| "matrix"  { K_MATRIX }
| "void"    { K_VOID }
| "break"   { K_BREAK }
| "continue"{ K_CONTINUE }
| "return"  { K_RETURN }
| "texture" { K_TEXTURE }
| "environment" { K_ENVIRONMENT }
| "shadow"  { K_SHADOW }
| "("  { OPAREN }
| ")"  { CPAREN }
| "{"  { OCURLY }
| "}"  { CCURLY }
| "["  { OSQUARE }
| "]"  { CSQUARE }
| ";"  { SEMI }
| ","  { COMMA }
| "-"  { MINUS }
| "+"  { PLUS }
| "*"  { TIMES }
| "/"  { DIV }
| "?"  { QUEST }
| ":"  { COLON }
| "."  { DOT }
| "^"  { CIRC }
| ">"  { GT }
| ">=" { GE }
| "<"  { LT }
| "<=" { LE }
| "==" { EQEQ }
| "!=" { NE }
| "&&" { AND }
| "||" { OR }
| "="  { EQ }
| "+=" { PLUSEQ }
| "-=" { MINUSEQ }
| "*=" { TIMESEQ }
| "/=" { DIVEQ }
| "!"  { EXCL }
| intrx { INT(int_of_string (Lexing.lexeme lexbuf)) }
| fltrx { FLOAT(float_of_string (Lexing.lexeme lexbuf)) }
| idrx  { ID(Lexing.lexeme lexbuf) }
| strrx { let str = Lexing.lexeme lexbuf in
          let str = String.sub str 1 (String.length str - 2) in
          STR(str) }
| eof   { EOF }
| ['\000'-'\255']     { failwith (Printf.sprintf "Lexer error: %s" (Lexing.lexeme lexbuf)) }
