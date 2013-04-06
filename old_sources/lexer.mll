{
    open Parser
    open Ast
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

| "mov"		{ OpCode(`MOV,  None) }
| "frac"	    { OpCode(`FRAC, None) }
| "add"		{ OpCode(`ADD,  None) }
| "mul"		{ OpCode(`MUL,  None) }
| "mad"		{ OpCode(`MAD,  None) }
| "dp3"		{ OpCode(`DP3,  None) }
| "dp2h"	    { OpCode(`DP2H, None) }
| "dph2"	    { OpCode(`DP2H, None) }
| "dp4"		{ OpCode(`DP4,  None) }
| "dp3h"	    { OpCode(`DP3H, None) }
| "dph3"    	{ OpCode(`DP3H, None) }

| "mov_sat"	{ OpCode(`MOV,  Some SAT) }
| "frac_sat"	{ OpCode(`FRAC, Some SAT) }
| "add_sat"	{ OpCode(`ADD,  Some SAT) }
| "mul_sat"	{ OpCode(`MUL,  Some SAT) }
| "mad_sat"	{ OpCode(`MAD,  Some SAT) }
| "dp3_sat"	{ OpCode(`DP3,  Some SAT) }
| "dp2h_sat"	{ OpCode(`DP2H, Some SAT) }
| "dph2_sat"	{ OpCode(`DP2H, Some SAT) }
| "dp4_sat"	{ OpCode(`DP4,  Some SAT) }
| "dp3h_sat"	{ OpCode(`DP3H, Some SAT) }
| "dph3_sat"	{ OpCode(`DP3H, Some SAT) }

| "mov_rcp"	{ OpCode(`MOV,  Some RCP) }
| "frac_rcp"	{ OpCode(`FRAC, Some RCP) }
| "add_rcp"	{ OpCode(`ADD,  Some RCP) }
| "mul_rcp"	{ OpCode(`MUL,  Some RCP) }
| "mad_rcp"	{ OpCode(`MAD,  Some RCP) }
| "dp3_rcp"	{ OpCode(`DP3,  Some RCP) }
| "dp2h_rcp"	{ OpCode(`DP2H, Some RCP) }
| "dph2_rcp"	{ OpCode(`DP2H, Some RCP) }
| "dp4_rcp"	{ OpCode(`DP4,  Some RCP) }
| "dp3h_rcp"	{ OpCode(`DP3H, Some RCP) }
| "dph3_rcp"	{ OpCode(`DP3H, Some RCP) }

| "mov_rsq"	{ OpCode(`MOV,  Some RSQ) }
| "frac_rsq"	{ OpCode(`FRAC, Some RSQ) }
| "add_rsq"	{ OpCode(`ADD,  Some RSQ) }
| "mul_rsq"	{ OpCode(`MUL,  Some RSQ) }
| "mad_rsq"	{ OpCode(`MAD,  Some RSQ) }
| "dp3_rsq"	{ OpCode(`DP3,  Some RSQ) }
| "dp2h_rsq"	{ OpCode(`DP2H, Some RSQ) }
| "dph2_rsq"	{ OpCode(`DP2H, Some RSQ) }
| "dp4_rsq"	{ OpCode(`DP4,  Some RSQ) }
| "dp3h_rsq"	{ OpCode(`DP3H, Some RSQ) }
| "dph3_rsq"	{ OpCode(`DP3H, Some RSQ) }

| "ld"		{ OpCodeD(`LOAD) }
| "load"    { OpCodeD(`LOAD) }
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
| "call"		{ K_CALL }
| "trace"	{ K_TRACE }
| "return"	{ K_RETURN }
| "exit"	    { K_EXIT }

| "par"		{ K_PAR }
| "or"		{ K_OR }
| "and"		{ K_AND }
| "push"		{ K_PUSH }
| "rty"		{ K_RTY }

| "("		{ PAREN_O }
| ")"		{ PAREN_C }
| "+"		{ PLUS }
| "-"		{ MINUS }
| "*"		{ STAR }
| ":"		{ COLON }
| ">="		{ GE }
| "<"		{ LT }
  
| ","		{ COMMA }
| "."		{ DOT }
| id			{ ID(Lexing.lexeme lexbuf) }

| number		{ NUM(float_of_string (Lexing.lexeme lexbuf)) }
| fpnum		{ NUM(float_of_string (Lexing.lexeme lexbuf)) }

| eof		{ EOF }
