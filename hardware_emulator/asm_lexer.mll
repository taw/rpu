{
    open Asm_parser
    (* open Ast *)
}
let space = [' ' '\t' '\n' '\r'] +
let comment = ";" ( [^'\n'] * ) '\n'
let alpha = ['a'-'z''A'-'Z''_']
let digit = ['0'-'9']
let alphanum = (alpha | digit)
let id = alpha ( alphanum * )
let number = '+' ? digit +
let fpnum = number "." number

rule token = parse
| space		{ token lexbuf }
| comment	{ token lexbuf }

| "mov"		{ OpCode(`MOV,  `NOP) }
| "frac"	{ OpCode(`FRAC, `NOP) }
| "add"		{ OpCode(`ADD,  `NOP) }
| "mul"		{ OpCode(`MUL,  `NOP) }
| "mad"		{ OpCode(`MAD,  `NOP) }
| "dp3"		{ OpCode(`DP3,  `NOP) }
| "dp2h"	{ OpCode(`DP2H, `NOP) }
| "dph2"	{ OpCode(`DP2H, `NOP) }
| "dp4"		{ OpCode(`DP4,  `NOP) }
| "dp3h"	{ OpCode(`DP3H, `NOP) }
| "dph3"    	{ OpCode(`DP3H, `NOP) }

| "mov_sat"	{ OpCode(`MOV,  `SAT) }
| "frac_sat"	{ OpCode(`FRAC, `SAT) }
| "add_sat"	{ OpCode(`ADD,  `SAT) }
| "mul_sat"	{ OpCode(`MUL,  `SAT) }
| "mad_sat"	{ OpCode(`MAD,  `SAT) }
| "dp3_sat"	{ OpCode(`DP3,  `SAT) }
| "dp2h_sat"	{ OpCode(`DP2H, `SAT) }
| "dph2_sat"	{ OpCode(`DP2H, `SAT) }
| "dp4_sat"	{ OpCode(`DP4,  `SAT) }
| "dp3h_sat"	{ OpCode(`DP3H, `SAT) }
| "dph3_sat"	{ OpCode(`DP3H, `SAT) }

| "mov_rcp"	{ OpCode(`MOV,  `RCP(3)) }
| "frac_rcp"	{ OpCode(`FRAC, `RCP(3)) }
| "add_rcp"	{ OpCode(`ADD,  `RCP(3)) }
| "mul_rcp"	{ OpCode(`MUL,  `RCP(3)) }
| "mad_rcp"	{ OpCode(`MAD,  `RCP(3)) }
| "dp3_rcp"	{ OpCode(`DP3,  `RCP(3)) }
| "dp2h_rcp"	{ OpCode(`DP2H, `RCP(3)) }
| "dph2_rcp"	{ OpCode(`DP2H, `RCP(3)) }
| "dp4_rcp"	{ OpCode(`DP4,  `RCP(3)) }
| "dp3h_rcp"	{ OpCode(`DP3H, `RCP(3)) }
| "dph3_rcp"	{ OpCode(`DP3H, `RCP(3)) }

| "mov_rcpx"	{ OpCode(`MOV,  `RCP(0)) }
| "frac_rcpx"	{ OpCode(`FRAC, `RCP(0)) }
| "add_rcpx"	{ OpCode(`ADD,  `RCP(0)) }
| "mul_rcpx"	{ OpCode(`MUL,  `RCP(0)) }
| "mad_rcpx"	{ OpCode(`MAD,  `RCP(0)) }
| "dp3_rcpx"	{ OpCode(`DP3,  `RCP(0)) }
| "dp2h_rcpx"	{ OpCode(`DP2H, `RCP(0)) }
| "dph2_rcpx"	{ OpCode(`DP2H, `RCP(0)) }
| "dp4_rcpx"	{ OpCode(`DP4,  `RCP(0)) }
| "dp3h_rcpx"	{ OpCode(`DP3H, `RCP(0)) }
| "dph3_rcpx"	{ OpCode(`DP3H, `RCP(0)) }

| "mov_rcpy"	{ OpCode(`MOV,  `RCP(1)) }
| "frac_rcpy"	{ OpCode(`FRAC, `RCP(1)) }
| "add_rcpy"	{ OpCode(`ADD,  `RCP(1)) }
| "mul_rcpy"	{ OpCode(`MUL,  `RCP(1)) }
| "mad_rcpy"	{ OpCode(`MAD,  `RCP(1)) }
| "dp3_rcpy"	{ OpCode(`DP3,  `RCP(1)) }
| "dp2h_rcpy"	{ OpCode(`DP2H, `RCP(1)) }
| "dph2_rcpy"	{ OpCode(`DP2H, `RCP(1)) }
| "dp4_rcpy"	{ OpCode(`DP4,  `RCP(1)) }
| "dp3h_rcpy"	{ OpCode(`DP3H, `RCP(1)) }
| "dph3_rcpy"	{ OpCode(`DP3H, `RCP(1)) }

| "mov_rcpz"	{ OpCode(`MOV,  `RCP(2)) }
| "frac_rcpz"	{ OpCode(`FRAC, `RCP(2)) }
| "add_rcpz"	{ OpCode(`ADD,  `RCP(2)) }
| "mul_rcpz"	{ OpCode(`MUL,  `RCP(2)) }
| "mad_rcpz"	{ OpCode(`MAD,  `RCP(2)) }
| "dp3_rcpz"	{ OpCode(`DP3,  `RCP(2)) }
| "dp2h_rcpz"	{ OpCode(`DP2H, `RCP(2)) }
| "dph2_rcpz"	{ OpCode(`DP2H, `RCP(2)) }
| "dp4_rcpz"	{ OpCode(`DP4,  `RCP(2)) }
| "dp3h_rcpz"	{ OpCode(`DP3H, `RCP(2)) }
| "dph3_rcpz"	{ OpCode(`DP3H, `RCP(2)) }

| "mov_rcpw"	{ OpCode(`MOV,  `RCP(3)) }
| "frac_rcpw"	{ OpCode(`FRAC, `RCP(3)) }
| "add_rcpw"	{ OpCode(`ADD,  `RCP(3)) }
| "mul_rcpw"	{ OpCode(`MUL,  `RCP(3)) }
| "mad_rcpw"	{ OpCode(`MAD,  `RCP(3)) }
| "dp3_rcpw"	{ OpCode(`DP3,  `RCP(3)) }
| "dp2h_rcpw"	{ OpCode(`DP2H, `RCP(3)) }
| "dph2_rcpw"	{ OpCode(`DP2H, `RCP(3)) }
| "dp4_rcpw"	{ OpCode(`DP4,  `RCP(3)) }
| "dp3h_rcpw"	{ OpCode(`DP3H, `RCP(3)) }
| "dph3_rcpw"	{ OpCode(`DP3H, `RCP(3)) }

| "mov_rsq"	{ OpCode(`MOV,  `RSQ(3)) }
| "frac_rsq"	{ OpCode(`FRAC, `RSQ(3)) }
| "add_rsq"	{ OpCode(`ADD,  `RSQ(3)) }
| "mul_rsq"	{ OpCode(`MUL,  `RSQ(3)) }
| "mad_rsq"	{ OpCode(`MAD,  `RSQ(3)) }
| "dp3_rsq"	{ OpCode(`DP3,  `RSQ(3)) }
| "dp2h_rsq"	{ OpCode(`DP2H, `RSQ(3)) }
| "dph2_rsq"	{ OpCode(`DP2H, `RSQ(3)) }
| "dp4_rsq"	{ OpCode(`DP4,  `RSQ(3)) }
| "dp3h_rsq"	{ OpCode(`DP3H, `RSQ(3)) }
| "dph3_rsq"	{ OpCode(`DP3H, `RSQ(3)) }

| "mov_rsqx"	{ OpCode(`MOV,  `RSQ(0)) }
| "frac_rsqx"	{ OpCode(`FRAC, `RSQ(0)) }
| "add_rsqx"	{ OpCode(`ADD,  `RSQ(0)) }
| "mul_rsqx"	{ OpCode(`MUL,  `RSQ(0)) }
| "mad_rsqx"	{ OpCode(`MAD,  `RSQ(0)) }
| "dp3_rsqx"	{ OpCode(`DP3,  `RSQ(0)) }
| "dp2h_rsqx"	{ OpCode(`DP2H, `RSQ(0)) }
| "dph2_rsqx"	{ OpCode(`DP2H, `RSQ(0)) }
| "dp4_rsqx"	{ OpCode(`DP4,  `RSQ(0)) }
| "dp3h_rsqx"	{ OpCode(`DP3H, `RSQ(0)) }
| "dph3_rsqx"	{ OpCode(`DP3H, `RSQ(0)) }

| "mov_rsqy"	{ OpCode(`MOV,  `RSQ(1)) }
| "frac_rsqy"	{ OpCode(`FRAC, `RSQ(1)) }
| "add_rsqy"	{ OpCode(`ADD,  `RSQ(1)) }
| "mul_rsqy"	{ OpCode(`MUL,  `RSQ(1)) }
| "mad_rsqy"	{ OpCode(`MAD,  `RSQ(1)) }
| "dp3_rsqy"	{ OpCode(`DP3,  `RSQ(1)) }
| "dp2h_rsqy"	{ OpCode(`DP2H, `RSQ(1)) }
| "dph2_rsqy"	{ OpCode(`DP2H, `RSQ(1)) }
| "dp4_rsqy"	{ OpCode(`DP4,  `RSQ(1)) }
| "dp3h_rsqy"	{ OpCode(`DP3H, `RSQ(1)) }
| "dph3_rsqy"	{ OpCode(`DP3H, `RSQ(1)) }

| "mov_rsqz"	{ OpCode(`MOV,  `RSQ(2)) }
| "frac_rsqz"	{ OpCode(`FRAC, `RSQ(2)) }
| "add_rsqz"	{ OpCode(`ADD,  `RSQ(2)) }
| "mul_rsqz"	{ OpCode(`MUL,  `RSQ(2)) }
| "mad_rsqz"	{ OpCode(`MAD,  `RSQ(2)) }
| "dp3_rsqz"	{ OpCode(`DP3,  `RSQ(2)) }
| "dp2h_rsqz"	{ OpCode(`DP2H, `RSQ(2)) }
| "dph2_rsqz"	{ OpCode(`DP2H, `RSQ(2)) }
| "dp4_rsqz"	{ OpCode(`DP4,  `RSQ(2)) }
| "dp3h_rsqz"	{ OpCode(`DP3H, `RSQ(2)) }
| "dph3_rsqz"	{ OpCode(`DP3H, `RSQ(2)) }

| "mov_rsqw"	{ OpCode(`MOV,  `RSQ(3)) }
| "frac_rsqw"	{ OpCode(`FRAC, `RSQ(3)) }
| "add_rsqw"	{ OpCode(`ADD,  `RSQ(3)) }
| "mul_rsqw"	{ OpCode(`MUL,  `RSQ(3)) }
| "mad_rsqw"	{ OpCode(`MAD,  `RSQ(3)) }
| "dp3_rsqw"	{ OpCode(`DP3,  `RSQ(3)) }
| "dp2h_rsqw"	{ OpCode(`DP2H, `RSQ(3)) }
| "dph2_rsqw"	{ OpCode(`DP2H, `RSQ(3)) }
| "dp4_rsqw"	{ OpCode(`DP4,  `RSQ(3)) }
| "dp3h_rsqw"	{ OpCode(`DP3H, `RSQ(3)) }
| "dph3_rsqw"	{ OpCode(`DP3H, `RSQ(3)) }

| "ld"		{ OpCodeD(`LOAD) }
| "load"        { OpCodeD(`LOAD) }
| "load4x"	{ OpCodeD(`LOAD4) }
| "ld4x"    	{ OpCodeD(`LOAD4) }
| "store"	{ OpCodeD(`STORE) }

| "texload"	{ OpCodeD(`TEXLOAD) }
| "texld_word_fp" { OpCodeD(`TEXLOAD) }
| "texload4x"	{ OpCodeD(`TEXLOAD4) }
| "texld4x"	{ OpCodeD(`TEXLOAD4) }
| "texld4x_word_fp" { OpCodeD(`TEXLOAD4) }
| "texstore"	{ OpCodeD(`TEXSTORE) }

| "if"		{ K_IF }
| "jmp"		{ K_JMP }
| "call"	{ K_CALL }
| "return"	{ K_RETURN }
| "exit"	{ K_EXIT }
| "trace"	{ OpCodeD(`TRACE) }

| "par"		{ K_PAR }
| "or"		{ K_OR }
| "and"		{ K_AND }
| "push"	{ K_PUSH }
| "rty"		{ K_RTY }

| "("		{ PAREN_O }
| ")"		{ PAREN_C }
| "+"		{ PLUS }
| "-"		{ MINUS }
| "*"		{ STAR }
| ":"		{ COLON }
| ">="		{ GE }
| "<="		{ LE }
| "<"		{ LT }
| ">"		{ GT }
| "="		{ EQ }
| "=="		{ EQ }
| "!="		{ NE }
  
| ","		{ COMMA }
| "."		{ DOT }
| id		{ ID(Lexing.lexeme lexbuf) }

| number	{ NUM(float_of_string (Lexing.lexeme lexbuf)) }
| fpnum		{ NUM(float_of_string (Lexing.lexeme lexbuf)) }

| eof		{ EOF }
