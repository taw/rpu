open Ast2
open Printf
open Util

(* MAIN CODE *)

let list_mapi f lst =
    let rec aux i = function
    | [] -> []
    | hd::tl -> let hdx = (f hd i) in hdx::(aux (i+1) tl)
    in
    aux 0 lst

let rec join sep = function
| []    -> ""
| a::[] -> a
| a::tl -> a^sep^(join sep tl)
let join0 lst = join "" lst
let join0_mapi f lst = join "" (list_mapi f lst)
let join0_map f lst = join "" (List.map f lst)

(* stubs *)

let escape_string s = "\""^s^"\"" (* FIXME: escape *)

(* MAIN CODE *)

(*
let addr_to_string (STATEMENT_INDEX i) = string_of_int i
*)

let var_typespec_to_string : var_typespec -> string
= function
| `FLOAT        -> "float"
| `COLOR        -> "color"
| `VEC(`POINT)  -> "point"
| `VEC(`VECTOR) -> "vector"
| `VEC(`NORMAL) -> "normal"
| `MATRIX       -> "matrix"
| `STRING       -> "string"
let fun_typespec_to_string : fun_typespec -> string
= function
| #var_typespec as x -> var_typespec_to_string x
| `VOID         -> "void"

let vti_to_string : vec_type -> string
= function
|`POINT  -> "point"
|`VECTOR -> "vector"
|`NORMAL -> "normal"

let flt_temp_to_string : flt_temp -> string
= function (`FLT_TEMP(x)) -> sprintf "t$%d:float" x

let vec_temp_to_string : vec_temp -> string
= function (`VEC_TEMP(x,vti)) -> sprintf "t$%d:%s" x (vti_to_string vti)

let col_temp_to_string : col_temp -> string
= function (`COL_TEMP(x)) -> sprintf "t$%d:color" x

let mtx_temp_to_string : mtx_temp -> string
= function (`MTX_TEMP(x)) -> sprintf "t$%d:matrix" x

let str_temp_to_string : str_temp -> string
= function (`STR_TEMP(x)) -> sprintf "t$%d:string" x

let any_temp_to_string : any_temp -> string
= function
| (#flt_temp as t) -> flt_temp_to_string t
| (#vec_temp as t) -> vec_temp_to_string t
| (#col_temp as t) -> col_temp_to_string t
| (#mtx_temp as t) -> mtx_temp_to_string t
| (#str_temp as t) -> str_temp_to_string t

let tuple_to_string : tuple -> string
= function vs ->
    sprintf "(%s)" (join ", " (List.map flt_temp_to_string vs))

let fun_call_to_string : fun_call -> string
= function (nam, args) ->
    sprintf "call(%s)" (join ", " (nam::(List.map any_temp_to_string args)))

let flt_comp_to_string : flt_comp -> string
= function
|`F_CONST(f)    -> sprintf "%f" f
|`F_ASG(t)      -> flt_temp_to_string t
|`F_ADD(a,b)    -> sprintf "%s + %s" (flt_temp_to_string a) (flt_temp_to_string b)
|`F_SUB(a,b)    -> sprintf "%s - %s" (flt_temp_to_string a) (flt_temp_to_string b)
|`F_MUL(a,b)    -> sprintf "%s * %s" (flt_temp_to_string a) (flt_temp_to_string b)
|`F_RCP(a)      -> sprintf "rcp(%s)" (flt_temp_to_string a)
|`F_RSQ(a)      -> sprintf "rsq(%s)" (flt_temp_to_string a)
|`F_ARR_LD(n,i) -> sprintf "%s[%s]" n (flt_temp_to_string i)
|`F_DOT_VV(a,b) -> sprintf "%s . %s" (vec_temp_to_string a) (vec_temp_to_string b)
|`F_DOT_CC(a,b) -> sprintf "%s . %s" (col_temp_to_string a) (col_temp_to_string b)
|`F_FUN(fc)     -> fun_call_to_string fc

let vec_comp_to_string : vec_comp -> string
= function
|`V_ASG(t)     -> vec_temp_to_string t
|`V_ADD(a,b)   -> sprintf "%s + %s" (vec_temp_to_string a) (vec_temp_to_string b)
|`V_SUB(a,b)   -> sprintf "%s - %s" (vec_temp_to_string a) (vec_temp_to_string b)
|`V_MUL(a,b)   -> sprintf "%s * %s" (vec_temp_to_string a) (vec_temp_to_string b)
|`V_XPD(a,b)   -> sprintf "%s ^ %s" (vec_temp_to_string a) (vec_temp_to_string b)
|`V_FLTCAST(t) -> flt_temp_to_string t
|`V_ARR_LD(n,i)-> sprintf "%s[%s]" n (flt_temp_to_string i)
|`V_FUN(fc)    -> fun_call_to_string fc
|`V_TUPLE(tp)  -> tuple_to_string tp

let col_comp_to_string : col_comp -> string
= function
|`C_ADD(a,b)   -> sprintf "%s + %s" (col_temp_to_string a) (col_temp_to_string b)
|`C_SUB(a,b)   -> sprintf "%s - %s" (col_temp_to_string a) (col_temp_to_string b)
|`C_MUL(a,b)   -> sprintf "%s * %s" (col_temp_to_string a) (col_temp_to_string b)
|`C_FUN(fc)    -> fun_call_to_string fc
|`C_TUPLE(tp)  -> tuple_to_string tp
|`C_ARR_LD(n,i)-> sprintf "%s[%s]" n (flt_temp_to_string i)
|`C_ASG(t)     -> col_temp_to_string t
|`C_FLTCAST(t) -> flt_temp_to_string t

let mtx_comp_to_string : mtx_comp -> string
= function
|`M_ADD(a,b)   -> sprintf "%s + %s" (mtx_temp_to_string a) (mtx_temp_to_string b)
|`M_SUB(a,b)   -> sprintf "%s - %s" (mtx_temp_to_string a) (mtx_temp_to_string b)
|`M_MUL(a,b)   -> sprintf "%s * %s" (mtx_temp_to_string a) (mtx_temp_to_string b)
|`M_TUPLE(tp)  -> tuple_to_string tp
|`M_ARR_LD(n,i)-> sprintf "%s[%s]" n (flt_temp_to_string i)
|`M_ASG(t)     -> mtx_temp_to_string t
|`M_FLTCAST(t) -> flt_temp_to_string t
|`M_FUN(fc)    -> fun_call_to_string fc

let str_comp_to_string : str_comp -> string
= function
|`S_CONST(c)   -> escape_string c
|`S_ARR_LD(n,i)-> sprintf "%s[%s]" n (flt_temp_to_string i)
|`S_ASG(t)     -> str_temp_to_string t
|`S_FUN(fc)    -> fun_call_to_string fc

let cond_to_string : cond -> string
= function
|`GT(a,b)  -> sprintf "%s > %s"  (flt_temp_to_string a) (flt_temp_to_string b)
|`GE(a,b)  -> sprintf "%s >= %s" (flt_temp_to_string a) (flt_temp_to_string b)
|`FEQ(a,b) -> sprintf "%s == %s" (flt_temp_to_string a) (flt_temp_to_string b)
|`VEQ(a,b) -> sprintf "%s == %s" (vec_temp_to_string a) (vec_temp_to_string b)
|`CEQ(a,b) -> sprintf "%s == %s" (col_temp_to_string a) (col_temp_to_string b)
|`MEQ(a,b) -> sprintf "%s == %s" (mtx_temp_to_string a) (mtx_temp_to_string b)
|`SEQ(a,b) -> sprintf "%s == %s" (str_temp_to_string a) (str_temp_to_string b)

let link_in_to_string : link_in -> string
= fun phi_in ->
    if phi_in = []
    then ""
    else sprintf "(%s)" (join ", " (List.map any_temp_to_string phi_in))

let link_out_to_string : link_out -> string
= fun (addr, phi_out) ->
    sprintf "%d%s"
    addr
    (if phi_out = []
    then ""
    else sprintf "(%s)" (join ", " (List.map any_temp_to_string phi_out))
    )

let op_to_string : op -> string
= function
|`NOP         -> "nop"
|`FCOMP(a,b)  -> sprintf "%s = %s" (flt_temp_to_string a) (flt_comp_to_string b)
|`VCOMP(a,b)  -> sprintf "%s = %s" (vec_temp_to_string a) (vec_comp_to_string b)
|`CCOMP(a,b)  -> sprintf "%s = %s" (col_temp_to_string a) (col_comp_to_string b)
|`MCOMP(a,b)  -> sprintf "%s = %s" (mtx_temp_to_string a) (mtx_comp_to_string b)
|`SCOMP(a,b)  -> sprintf "%s = %s" (str_temp_to_string a) (str_comp_to_string b)
|`PROCCALL(fc)-> fun_call_to_string fc
|`CJMP(c,lo)  -> sprintf "if (%s) jump %s; " (cond_to_string c) (link_out_to_string lo)

let vardef_to_string : vardef -> string
= function (name,(is_extern,is_array,typespec,sem_info)) ->
    sprintf "var %s : %s%s%s%s"
    name
    (if is_extern then "extern " else "")
    (match sem_info with
    | `LOCAL -> ""
    | `MAGIC(s) -> sprintf "magic(%s) " s
    | `PARAM(i) -> sprintf "param[%d] " i
    )
    (var_typespec_to_string typespec)
    (if is_array then "[]" else "")

let vardefs_to_string : vardef list -> string
= function vardefs ->
    join0 (List.map (fun vardef -> "    "^(vardef_to_string vardef)^"\n") vardefs)

let statement_to_string : int -> statement_cnt -> string
= fun label (link_in, stmt, link_out) ->
    let stmt_s = op_to_string stmt in
    (sprintf "    %d%s: %s -> %s\n" label (link_in_to_string link_in) stmt_s (link_out_to_string link_out))    

let body_to_string_physical : body -> string
= function (stmts_ht,variables,init_label,exit_label) ->
    let stmts = ref [] in
    let _ = ht_sorted_iter (fun label stmt ->
		stmts := (statement_to_string label stmt)::!stmts
    ) stmts_ht in
    sprintf "    start at %d\n%s%s    end at %d\n"
	init_label
	(vardefs_to_string variables)
	(join0 !stmts)
	exit_label

let body_to_string_logical : body -> string
= function (stmts_ht,variables,init_label,exit_label) ->
    let visited_stmts = set_new () in
    let stmts = ref [] in
    let rec visit_stmt label = 
	if not (set_mem visited_stmts label)
	then
	(
	    set_set visited_stmts label;
	    if ht_mem stmts_ht label
	    then
	    (
		let stmt = ht_get stmts_ht label in
	        let stmt_s = statement_to_string label stmt in	
	        stmts := stmt_s::!stmts;
		match stmt with (* FIXME: handle `RETURN too ? *)
		| (_, `CJMP(_, (if_target,_)), (else_target, _)) ->
		    (visit_stmt if_target; visit_stmt else_target)
		| (_, _, (jmp_target, _)) -> visit_stmt jmp_target
	    )
	    else
	    (
		stmts := (sprintf "    %d: END\n" label)::!stmts
	    )
	)
    in
    (* FIXME: What about dead code ?*)
    visit_stmt init_label;
    sprintf "    start at %d\n%s%s"
	init_label
	(vardefs_to_string variables)
	(join0 (List.rev !stmts))

(*
FIXME: Comment it out by default until fixed
let body_to_string = body_to_string_physical
*)
let body_to_string = body_to_string_logical

let param_to_string (fo,fts,name,e) = sprintf "%s%s %s%s"
                                      (if fo then "output " else "") 
				      (var_typespec_to_string fts)
				      name
				      (match e with
				      | None -> ""
				      | Some _ -> " = ?")

let params_to_string : param list -> string
= function
| []     -> ""
| params -> "\n" ^ join0 (List.map (fun (`FORMAL (fo,fts,name,e)) -> "    " ^ (param_to_string (fo,fts,name,e)) ^ ";\n") params)

let shader_type_to_string : shader_type -> string
= function
| `SURFACE      -> "surface"
| `IMAGER       -> "imager"
| `DISPLACEMENT -> "displacement"
| `VOLUME       -> "volume"
| `LIGHT        -> "light"

let function_return_type_to_string = fun_typespec_to_string

let prelude_to_string : prelude -> string =
fun (params, prelude_elements) ->
	(join0_mapi (fun temp i ->
	    sprintf "    param[%d] -> %s\n" i (any_temp_to_string temp);
	) params) ^
	(join0_map (function
	|`EXTERN(nam,temp) ->
	    sprintf "    extern(%s) -> %s\n" nam (any_temp_to_string temp);
	|`MAGIC(nam,temp)  ->
	    sprintf "    magic(%s) -> %s\n" nam (any_temp_to_string temp);
	) prelude_elements)

let epilogue_to_string : epilogue -> string =
fun (phi_in, retval, epilogue_elements) ->
    if phi_in = [] && retval = None && epilogue_elements = []
    then
	""
    else
	sprintf "%s%s%s"
	(
	if phi_in = []
	then ""
	else sprintf "    phi_in(%s)\n" (list_join ", " (List.map (any_temp_to_string) phi_in))
	)
	(match retval with
	| None -> ""
	| Some temp -> sprintf "    return %s\n" (any_temp_to_string temp);
	)
	(join0_map (function
	|`EXTERN(nam,temp) ->
	    sprintf "    %s -> extern(%s)\n" (any_temp_to_string temp) nam;
	|`MAGIC(nam,temp)  ->
	    sprintf "    %s -> magic(%s)\n" (any_temp_to_string temp) nam;
	|`OUTPUT(i,temp) ->
	    sprintf "    %s -> output_param[%d]\n" (any_temp_to_string temp) i;
	) epilogue_elements)

let definition_to_string : definition -> string
= function
| `SHADER(st,name,params,prelude,body,epilogue) ->
	sprintf "%s %s(%s) {\nprelude:\n%s\nbody:\n%s\nepilogue:\n%s}\n"
        (shader_type_to_string st)
	name
	(params_to_string params)
	(prelude_to_string prelude)
	(body_to_string body)
	(epilogue_to_string epilogue)
| `FUNCTION(rt,name,params,prelude,body,epilogue) ->
	sprintf "%s %s(%s) {\nprelude:\n%s\nbody:\n%s\nepilogue:\n%s}\n"
        (function_return_type_to_string rt)
	name
	(params_to_string params)
	(prelude_to_string prelude)
	(body_to_string body)
	(epilogue_to_string epilogue)

let ast2_to_string : Ast2.definition list -> string =
function definitions ->
    join "\n" (List.map definition_to_string definitions)
