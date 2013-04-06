open Ast2
open Util
open Compiler_state
open Printf
open Print_ast2

(********************************************************************)
(* Main functions                                                   *)
(********************************************************************)
let compile_function : (Ast.function_return_type * string * Ast.param list * Ast.body) ->
                       (function_return_type * string * param list * prelude * body * epilogue)
= function (rt,name,params,body) ->
    let state = new compiler_state in
    state#init_function rt;
    state#register_params params;
    Expand_ast.function_abi_register (name, rt, params);
    let thr = state#codestart_thr in
    Expand_ast.compile_statement (`BLOCK body, thr);
    state#finalize thr;
    let prelude = (da_to_list state#params), (da_to_list state#magic_and_extern) in
    let epilogue = (state#epilogue_phi_in), (state#return), (da_to_list state#writeback) in
    let statements = state#extract_statements in
    (
    rt,
	name,
	params,
	prelude,
	(statements, [], state#preinit_cpt#i, state#codeend_cpt#i),
	epilogue
    )

let compile_shader : (Ast.shader_type * string * Ast.param list * Ast.body) ->
		     (shader_type * string * param list * prelude * body * epilogue)
= function (st,name,params,body) ->
    let state = new compiler_state in
    state#init_shader st;
    state#register_params params;
    Expand_ast.shader_abi_register (name, st);
    let thr = state#codestart_thr in
    Expand_ast.compile_statement (`BLOCK body, thr);
    state#finalize thr;
    let prelude = (da_to_list state#params), (da_to_list state#magic_and_extern) in
    let epilogue = (state#epilogue_phi_in), (state#return), (da_to_list state#writeback) in
    let statements = state#extract_statements in
    (
    st,
	name,
	params,
	prelude,
	(statements, [], state#preinit_cpt#i, state#codeend_cpt#i),
	epilogue
    )

let compile_toplevel_definition : Ast.definition -> Ast2.definition
= function
| `FUNCTION(a,b,c,d) -> `FUNCTION (compile_function (a,b,c,d))
| `SHADER(a,b,c,d)   -> `SHADER   (compile_shader   (a,b,c,d))

let compile : Ast.definition list -> Ast2.definition list =
    function ast ->
    List.map compile_toplevel_definition ast
