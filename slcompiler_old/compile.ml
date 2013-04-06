open Ast2
open Util
open Compiler_state
open Printf
open Print_ast2

(*let extract_variables state  = hashtbl_contents state#variables*)

(********************************************************************)
(* Optimizations                                                    *)
(********************************************************************)

(*
 * It's quite sad, expand_ast.ml tries to hard not to produce any
 * dead code (the whole dev_null framework), but eliminate_nops
 * would get too complicated if it was held it to the same standards,
 * and I'm too tired to.
 *
 * Ssa.convert_to_ssa ABSOLUTELY REQUIRES that all code is reachable,
 * otherwise it raises an exception.
 *)

let unreachable_code_elimination (state:compiler_state) =
    let reachable_code = set_new () in
    let rec reach i =
	if set_mem reachable_code i
	then
	    ()
	else
	(
	    set_set reachable_code i;
	    (new statement state i)#iterate_edges (fun edge ->
		reach (edge#dst)
	    )
	)
    in
    reach (state#preinit_cpt#i);
    state#iterate_statements (fun s ->
	if not (set_mem reachable_code (s#i))
	then
	    s#drop
    )

(*
 * Eliminate NOPs (actually only NOPs without non-trivial linkouts/linkins).
 * Also eliminates CJMPs both targets of which are identical
 *
 * I'm not 100% sure why did I even code such a weak optimization ^_^
 * It's probably going to be subsumed by something one million times stronger
 *)
let rec eliminate_nops (state:compiler_state) =
begin
(*    printf "Run eliminate-nops\n";*)
    let restart_analysis = ref false in
    let nops = da_new () in
    let nop_targets = ht_new () in
    state#iterate_statements (fun stmt ->
	match stmt#statement with
	| ([], `NOP, link_out) ->
	    da_push nops (stmt#i);
	    ht_set nop_targets (stmt#i) link_out
	| _ -> ()
    )
    ;
    da_iter (fun nop ->
	let nop_link_out = ht_get nop_targets nop in
	if (nop,[]) = nop_link_out
	then
	    ()
	    (* NOP that points to itself, like while(1>0) {}
	     * It cannot "really" happen yet
	     *)
	else
	    let updates = ht_collect (fun i op ->
	    match op with
	    | (phi_in,`CJMP(cond,if_link),else_link) ->
		let new_if_link   = if if_link   = (nop,[]) then nop_link_out else if_link in
		let new_else_link = if else_link = (nop,[]) then nop_link_out else else_link in
		if new_if_link = new_else_link then
		(
		    (* restart_analysis := true; *) (* Every update triggers a restart anyway *)
		    Some (i,(phi_in,`NOP,new_else_link)) (* A really stupid place to get rid of useless checks *)
		)
		(* Do not update if nothing changed *)
		else if (new_if_link,new_else_link) = (if_link,else_link) then
		    None
		else
		    Some (i,(phi_in,`CJMP(cond, new_if_link),new_else_link))
	    | (phi_in,op,link_out) ->
		if link_out = (nop, [])
		then
		    Some (i, (phi_in, op, nop_link_out))
		else
		    None
	    ) state#stmts in
	    List.iter (fun (i,op) ->
		restart_analysis := true;
		(new codepoint state i)#replace_stmt op
	    ) updates;
	    if state#preinit_cpt#i = nop
	    then state#set_preinit_cpt (new codepoint state (match nop_link_out with (jmp_target,_) -> jmp_target));
	    (* (new codepoint state nop)#remove_stmt *)
    ) nops;
    (*
     * A: cjmp cond, B; -> C
     * B: nop -> C
     * C: ...
     *
     * becomes
     * A: nop -> C
     * C: ...
     *
     * just rerun the analysis
     *)
    if !restart_analysis
    then
	eliminate_nops state
    else
	unreachable_code_elimination state
end

(*
 * That's a pretty simple optimization, just to get something fast
 * before moving on to register allocation and code generation.
 * Basically, we want to get rid of all copies.
 *)
let copy_propagation (state:compiler_state) =
begin
    let copy_defs : (any_temp, (any_temp*statement)) ht = ht_new () in
    let propagate : (any_temp, any_temp) ht = ht_new () in
    (* ASSUMPTION: The assignments can't be cyclic *)
    let rec compute_propagate (dst:any_temp) =
	if ht_mem copy_defs dst
	then
	    if ht_mem propagate dst
	    then (* dst has already been computed *)
		ht_get propagate dst
	    else
	    (
		let real_src = compute_propagate (let (temp, _) = ht_get copy_defs dst in temp) in
		ht_set propagate dst real_src;
		real_src
	    )
	else (* dst is not a copy *)
	    dst
    in
    (* First extract copy statements *)
    state#iterate_statements (fun stmt ->
	match stmt#op with
	| `FCOMP(dst_temp, `F_ASG(src_temp)) -> ht_set copy_defs (dst_temp :> any_temp) ((src_temp :> any_temp), stmt)
	| `VCOMP(dst_temp, `V_ASG(src_temp)) -> ht_set copy_defs (dst_temp :> any_temp) ((src_temp :> any_temp), stmt)
	| `CCOMP(dst_temp, `C_ASG(src_temp)) -> ht_set copy_defs (dst_temp :> any_temp) ((src_temp :> any_temp), stmt)
	| `MCOMP(dst_temp, `M_ASG(src_temp)) -> ht_set copy_defs (dst_temp :> any_temp) ((src_temp :> any_temp), stmt)
	| `SCOMP(dst_temp, `S_ASG(src_temp)) -> ht_set copy_defs (dst_temp :> any_temp) ((src_temp :> any_temp), stmt)
	| _ -> ()
	;
    );
    (* Now, the idea is that copies could be chained, even O(n),
     * and we want to get to do everything in a single copy_propagation pass
     *
     *)
    ht_iter (fun k _ -> ignore(compute_propagate k)) copy_defs;
(*
    ht_iter (fun dst (src, stmt) ->
        printf "COPY: %s <- %s at %d\n"
	       (any_temp_to_string dst)
	       (any_temp_to_string src)
	       (stmt#i)
    ) copy_defs;
    ht_iter (fun dst src ->
        printf "PROP: %s <- %s\n"
	       (any_temp_to_string dst)
	       (any_temp_to_string src)
    ) propagate;
*)
    state#global_temp_use_replace(fun temp ->
	if ht_mem propagate temp
	then ht_get propagate temp
	else temp
    );
    (* Now we have some easy dead code to eliminate,
     * namely the assignments to temporaries that we just killed *)
    ht_iter (fun _ (_, stmt) ->	stmt#make_nop) copy_defs;
end

(*
 * Compute useful temporaries, then drop all that aren't.
 * As unreachable code elimination is run before this,
 * many temporaries that seem useful but really aren't get eliminated too.
 *)

type any_comp = [flt_comp | vec_comp | col_comp | mtx_comp | str_comp]

let eliminate_useless_temporaries (state:compiler_state) =
begin
    let used : any_temp set = set_new () in
    let propagations : (any_temp, any_temp) mht = mht_new () in
    let used_in_computation : (any_comp -> any_temp list) = fun comp -> match comp with
    |`F_CONST(_)    -> []
    |`F_ASG(t)      -> [(t :> any_temp)]
    |`F_ARR_LD(_,i) -> [(i :> any_temp)]
    |`F_ADD(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`F_SUB(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`F_MUL(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`F_RCP(a)      -> [(a :> any_temp)]
    |`F_RSQ(a)      -> [(a :> any_temp)]
    |`F_DOT_VV(a,b) -> [(a :> any_temp);(b :> any_temp)]
    |`F_DOT_CC(a,b) -> [(a :> any_temp);(b :> any_temp)]
    |`F_FUN(_,_)    -> []

    |`V_TUPLE(vs)   -> (vs :> any_temp list)
    |`V_ASG(t)      -> [(t :> any_temp)]
    |`V_ARR_LD(_,i) -> [(i :> any_temp)]
    |`V_FLTCAST(t)  -> [(t :> any_temp)]
    |`V_ADD(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`V_SUB(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`V_MUL(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`V_XPD(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`V_FUN(_,_)    -> []

    |`C_TUPLE(vs)   -> (vs :> any_temp list)
    |`C_ASG(t)      -> [(t :> any_temp)]
    |`C_ARR_LD(_,i) -> [(i :> any_temp)]
    |`C_FLTCAST(t)  -> [(t :> any_temp)]
    |`C_ADD(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`C_SUB(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`C_MUL(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`C_FUN(_,_)    -> []

    |`M_TUPLE(vs)   -> (vs :> any_temp list)
    |`M_ASG(t)      -> [(t :> any_temp)]
    |`M_ARR_LD(_,i) -> [(i :> any_temp)]
    |`M_FLTCAST(t)  -> [(t :> any_temp)]
    |`M_ADD(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`M_SUB(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`M_MUL(a,b)    -> [(a :> any_temp);(b :> any_temp)]
    |`M_FUN(_,_)    -> []

    |`S_CONST(_)    -> []
    |`S_ASG(t)      -> [(t :> any_temp)]
    |`S_FUN(_,_)    -> []
    |`S_ARR_LD(_,i) -> [(i :> any_temp)]
    in
    let rec propagate used_temp =
	if set_mem used used_temp
	then
	    ()
	else
	(
	    set_set used used_temp;
	    List.iter (fun temp ->
			propagate temp
	    ) (mht_get_all propagations used_temp)
	)
    in
    (* A temporary is used if it's returned from the fuction (as return or writeback)
     * or for control flow, or to compute an used temporary.
     *
     * First, compute propagations (if a is used then b is used)
     * Then, recursively compute a set of used temporaries, starting with 
     *       the ones that are used directly (control flow or returned)
     *)
    state#iterate_statements (fun s ->
	(match s#op with
	|`NOP -> ()
	|`CJMP(_,_) -> ()
	|`FCOMP(temp,comp) -> List.iter (fun prop_temp -> mht_add propagations (temp :> any_temp) prop_temp)
	                                (used_in_computation (comp :> any_comp))
	|`VCOMP(temp,comp) -> List.iter (fun prop_temp -> mht_add propagations (temp :> any_temp) prop_temp)
	                                (used_in_computation (comp :> any_comp))
	|`CCOMP(temp,comp) -> List.iter (fun prop_temp -> mht_add propagations (temp :> any_temp) prop_temp)
	                                (used_in_computation (comp :> any_comp))
	|`MCOMP(temp,comp) -> List.iter (fun prop_temp -> mht_add propagations (temp :> any_temp) prop_temp)
	                                (used_in_computation (comp :> any_comp))
	|`SCOMP(temp,comp) -> List.iter (fun prop_temp -> mht_add propagations (temp :> any_temp) prop_temp)
	                                (used_in_computation (comp :> any_comp))
	|`PROCCALL(_,_) -> ()
	);
	s#iterate_edges(fun edge ->
	    let phi_out = edge#phi_out in
	    let phi_in  = edge#phi_in in
	    List.iter2 (fun tmp_out tmp_in ->
		mht_add propagations tmp_in tmp_out
	    ) phi_out phi_in
	)
    );
    (* ht_iter (fun a b -> printf "%s needs %s\n" (any_temp_to_string a) (any_temp_to_string b)) propagations;*)
    (* Now iterate everything that is directly useful ... *)
    (match state#return with
    | Some temp -> propagate temp
    | None -> ()
    );
    da_iter (function
    |`MAGIC (_,temp) -> propagate temp
    |`OUTPUT(_,temp) -> propagate temp
    |`EXTERN(_,temp) -> propagate temp
    ) state#writeback;
    state#iterate_statements(fun s ->
	(match s#op with
	|`CJMP(`GT(a,b),_) -> List.iter propagate [(a :> any_temp); (b :> any_temp)]
	|`CJMP(`GE(a,b),_) -> List.iter propagate [(a :> any_temp); (b :> any_temp)]
	|`CJMP(`FEQ(a,b),_) -> List.iter propagate [(a :> any_temp); (b :> any_temp)]
	|`CJMP(`VEQ(a,b),_) -> List.iter propagate [(a :> any_temp); (b :> any_temp)]
	|`CJMP(`CEQ(a,b),_) -> List.iter propagate [(a :> any_temp); (b :> any_temp)]
	|`CJMP(`MEQ(a,b),_) -> List.iter propagate [(a :> any_temp); (b :> any_temp)]
	|`CJMP(`SEQ(a,b),_) -> List.iter propagate [(a :> any_temp); (b :> any_temp)]
	|`FCOMP(_,`F_FUN(_,args)) -> List.iter propagate args
	|`VCOMP(_,`V_FUN(_,args)) -> List.iter propagate args
	|`CCOMP(_,`C_FUN(_,args)) -> List.iter propagate args
	|`MCOMP(_,`M_FUN(_,args)) -> List.iter propagate args
	|`SCOMP(_,`S_FUN(_,args)) -> List.iter propagate args
	|`FCOMP(_,_) -> ()
	|`VCOMP(_,_) -> ()
	|`CCOMP(_,_) -> ()
	|`MCOMP(_,_) -> ()
	|`SCOMP(_,_) -> ()
	|`PROCCALL(_,args) -> List.iter propagate args
	|`NOP -> ()
	);
    );
    (* ht_iter (fun temp _ -> printf "%s is useful\n" (any_temp_to_string temp)) used;*)
    (* So we know which definitions are useful. Now drop those that aren't
     * There are 4 kinds of definitions:
     * * prelude definitions    - DROP
     * * t = phi(...)           - DROP
     * * t = computation(...)   - DROP
     * * t = fun(...)           - convert to `PROCCALL(...)
     *)
    state#iterate_statements(fun s ->
	(match s#op with
	|`FCOMP(t,`F_FUN(nam,args)) -> (if not (set_mem used (t :> any_temp)) then (s#set_op (`PROCCALL(nam,args))))
	|`VCOMP(t,`V_FUN(nam,args)) -> (if not (set_mem used (t :> any_temp)) then (s#set_op (`PROCCALL(nam,args))))
	|`CCOMP(t,`C_FUN(nam,args)) -> (if not (set_mem used (t :> any_temp)) then (s#set_op (`PROCCALL(nam,args))))
	|`MCOMP(t,`M_FUN(nam,args)) -> (if not (set_mem used (t :> any_temp)) then (s#set_op (`PROCCALL(nam,args))))
	|`SCOMP(t,`S_FUN(nam,args)) -> (if not (set_mem used (t :> any_temp)) then (s#set_op (`PROCCALL(nam,args))))
	|`FCOMP(t,_) -> (if not (set_mem used (t :> any_temp)) then ((*(printf "Killing %s\n" (any_temp_to_string (t :> any_temp)));*) s#make_nop ))
	|`VCOMP(t,_) -> (if not (set_mem used (t :> any_temp)) then ((*(printf "Killing %s\n" (any_temp_to_string (t :> any_temp)));*) s#make_nop ))
	|`CCOMP(t,_) -> (if not (set_mem used (t :> any_temp)) then ((*(printf "Killing %s\n" (any_temp_to_string (t :> any_temp)));*) s#make_nop ))
	|`MCOMP(t,_) -> (if not (set_mem used (t :> any_temp)) then ((*(printf "Killing %s\n" (any_temp_to_string (t :> any_temp)));*) s#make_nop ))
	|`SCOMP(t,_) -> (if not (set_mem used (t :> any_temp)) then ((*(printf "Killing %s\n" (any_temp_to_string (t :> any_temp)));*) s#make_nop ))
	|`PROCCALL(_,_) -> ()
	|`NOP -> ()
	|`CJMP(_,_) -> ()
	);
	s#iterate_edges(fun edge ->
	    let phi_out = edge#phi_out in
	    let phi_in  = edge#phi_in in
	    let new_phi_out = da_new () in
	    (* We remove source if *target* is not used,
	       source may be used somewhere else after all *)
	    List.iter2 (fun temp_out temp_in ->
		if (set_mem used temp_in)
		then da_push new_phi_out temp_out
		else (*printf "Killing phi-out %s to %s\n" (any_temp_to_string temp_out) (any_temp_to_string temp_in)*) ()
	    ) phi_out phi_in;
	    edge#set_phi_out (da_to_list new_phi_out)
	)
    );
    state#iterate_statements(fun s ->
	(*printf "Trying to get rid of useless phi-in of %d\n" s#i;*)
	let phi_in = s#phi_in in
	let new_phi_in = List.filter (fun phi_in_temp -> set_mem used phi_in_temp) phi_in in
	(*List.iter (fun temp ->
	    printf "\t%s %s\n" (any_temp_to_string temp) (if (set_mem used temp) then "USED" else "NOT")
	) phi_in;*)
	s#set_phi_in new_phi_in
    );
    da_imperative_filter (function
    |`MAGIC(_,temp)  -> set_mem used temp
    |`EXTERN(_,temp) -> set_mem used temp
    ) state#magic_and_extern
end

let run_basic_optimization state =
begin
(* Remove stuff introduced by the compiler *)
    eliminate_nops state;
    Ssa.convert_to_ssa state;
    copy_propagation state;
    eliminate_useless_temporaries state;
(* Remove stuff nop-ified by copy propagation and other optimizations *)
    eliminate_nops state;
end

(********************************************************************)
(* Main functions                                                   *)
(********************************************************************)
let compile_function : (Ast.function_return_type * string * Ast.param list * Ast.body) ->
                       (function_return_type * string * param list * prelude * body * epilogue)
= function (rt,name,params,body) ->
    let state = new compiler_state in
    state#init_function rt;
    state#register_params params;
    let thr = state#codestart_thr in
    Expand_ast.compile_statement (`BLOCK body, thr);
    state#finalize thr;
    run_basic_optimization state;
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
    let thr = state#codestart_thr in
    Expand_ast.compile_statement (`BLOCK body, thr);
    state#finalize thr;
    run_basic_optimization state;
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
