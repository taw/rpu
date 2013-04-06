(***********************************************)
(* This file contains functions that convert   *)
(* the code to SSA form                        *)
(***********************************************)

open Printf
open Util
open Ast2
open Compiler_state

let temp_type : any_temp -> [>var_typespec] = function
| `FLT_TEMP(_) -> `FLOAT
| `VEC_TEMP(_,vti) -> `VEC(vti)
| `COL_TEMP(_) -> `COLOR
| `MTX_TEMP(_) -> `MATRIX
| `STR_TEMP(_) -> `STRING

(*
let add_phi_out : (int*any_temp) -> int -> unit
= fun (from_i, from_temp) to_i ->
    if from_i = -1
    then
	failwith "FIXME: Phi-link from prelude not supported yet"
    else
    if STATEMENT_INDEX(from_i) = state.codeend_label
    then
	failwith "Internal error: Phi-link from epilogue"
    else
    (
    let (link_in, op, (jmp_target, phi_out)) = get_stmt from_i in
    let new_op =
    (match op with
    | `CJMP(cond, (if_jmp_target, if_phi_out)) ->
	if jmp_target <> STATEMENT_INDEX(to_i) && if_jmp_target <> STATEMENT_INDEX(to_i)
	then
	    failwith (sprintf
	    "Internal error: Trying to add phi-out at a CJMP node(%d) that points at %d and %d, not at the right phi-in(%d)"
	    from_i
	    (match if_jmp_target with STATEMENT_INDEX(x) -> x)
	    (match jmp_target with STATEMENT_INDEX(x) -> x)
	    to_i
	    );
	let new_if_phi_out = (if if_jmp_target = STATEMENT_INDEX(to_i) then from_temp::if_phi_out else if_phi_out) in
	let new_else_phi_out = (if jmp_target = STATEMENT_INDEX(to_i) then from_temp::phi_out else phi_out) in
	(link_in, `CJMP(cond, (if_jmp_target, new_if_phi_out)), (jmp_target, new_else_phi_out))
    | _ ->
	if jmp_target <> STATEMENT_INDEX(to_i)
	then failwith (sprintf
	    "Internal error: Trying to add phi-out at a node(%d) that points at %d, not at the right phi-in(%d)"
	    from_i
	    (match jmp_target with STATEMENT_INDEX(x) -> x)
	    to_i
	    );
	(link_in, op, (jmp_target, from_temp::phi_out))
    )
    in
    ht_set state.stmts from_i new_op
    )
*)

(*
let add_phi_in : (int*any_temp) -> unit
= fun (i, ssa_temp) ->
    if i = -1
    then
	failwith "Internal error: Phi-link to prelude"
    else
    if STATEMENT_INDEX(i) = state.codeend_label
    then
    (
    
    )
    else
    let (link_in, op, link_out) = get_stmt i in
    ht_set state.stmts i (ssa_temp::link_in, op, link_out)
*)

(*
 * Computes each node's inputs and converts to SSA form.
 *)
let convert_to_ssa_real (state:compiler_state) =
begin
    let inputs : (int, edge) mht = mht_new () in
    let temp_definitions : (any_temp, statement) mht = mht_new () in
    state#iterate_statements (fun stmt ->
	    stmt#iterate_edges (fun edge ->
	        mht_add inputs (edge#dst) edge;
		(*printf "Added %s to inputs\n" edge#to_s;*)
	    );
	    stmt#iterate_normal_defs (fun temp ->
		mht_add temp_definitions temp stmt
	    );
    )
    ;
    begin (* compile prelude *)
	let prelude_edge = state#prelude_edge in
	mht_add inputs (prelude_edge#dst) prelude_edge;
	da_iter (fun temp -> 
	    mht_add temp_definitions temp (new statement state (-1))
	) state#params;
	da_iter (function
	| `MAGIC(_, temp) ->
	    mht_add temp_definitions temp (new statement state (-1))
	| `EXTERN(_, temp) ->
	    mht_add temp_definitions temp (new statement state (-1))
	) state#magic_and_extern
    end
    ;
    let temps_that_need_ssa = da_new () in
    mht_iter_all (fun temp defs ->
	if List.length defs > 1
	then
	(
	    da_push temps_that_need_ssa temp;
	    (* printf "%s has %d definitions\n" (Print_ast2.any_temp_to_string temp) (List.length defs) *)
	)
	else
	    ()
    ) temp_definitions;
    (*
     * So we have a temporary that needs to be split.
     * What we need is:
     * * insert some phi-definitions here and there
     * * at each point know to which definition does temp refer to
     * * replace all uses of the original by uses of the new SSA definitions
     * * replace all definitions by SSA definitions
     *)
    da_iter (fun temp ->
	let original_definition_points = da_of_list (mht_get_all temp_definitions temp) in
	let definitions          = da_new () in
	let new_definition_temps = ht_new () in
	let typ = temp_type temp in
	da_iter (fun def_point ->
	    let new_temp = state#new_any_temp typ in
	    ht_set new_definition_temps (true, def_point) new_temp;
	    da_push definitions (true, def_point)
	) original_definition_points
	;
	let phi : (int, any_temp) ht = ht_new () in
	(*
	 * Each iteration:
	 * * computes reachability
	 * * inserts phi at dominance frontier
	 * If any phi have been inserted, rerun
	 *)	
	let rec iteration () =
	    let needs_a_rerun = ref false in
	    (*
	     * We do not physically insert PHI yet. So there are actually
	     * 3 reachabilities we need to care about not 2:
	     *
	     * --> [    |         |           ]
	     *     [phi | compute | writeback ] -->
	     * --> [    |         |           ]
	     * ^^^      ^^^^^^^^^^^             ^^^
	     * entry     inside                 exit
	     *
	     * entry/inside distinction also affects the epilogue !
	     * reachability_inside is used to disambiguate the computations
	     * phi and writeback/prologue are corrected separately
	     *
	     * Reachability is (statement_index -> any_temp) map
	     * where statement_index may have special values -1 for prologue
	     * and state.exit_label for epilogue,
	     * and any_temp is a newly introduced SSA temporary
	     * We cannot use temporary's definition point as it is not unique
	     * in case of PHI introduced to the same cell as a definition.
	     *)
	    let reachability_at_entry : (int, any_temp) mht = mht_new () in
	    let reachability_inside   : (int, any_temp) mht = mht_new () in
	    let reachability_at_exit  : (int, any_temp) mht = mht_new () in
	    (* visited during propagation of current definition *)
	    let visited               = set_new () in
	    let rec reachable_at_entry_of ssa_temp (s:statement) =
		if set_mem visited (s#i)
	        then ()
		else
	        (
		    set_set visited (s#i);
		    mht_add reachability_at_entry (s#i) ssa_temp;
		    if ht_mem phi (s#i)
		    then ()
		    else reachable_inside_of ssa_temp s
		)
	    and reachable_inside_of (ssa_temp:any_temp) (s:statement) =
	    (* No duplicate entry check here, test if phi in entry_of is enough *)
		mht_add reachability_inside (s#i) ssa_temp;
		if s#is_prelude
	    	then
		    failwith "Internal error: tried to enter the prelude"
		else if s#is_epilogue
		then
		    reachable_at_exit_of ssa_temp s
		    (* WAS: Do not propagate over phi or end of function *)
		else
		(
	    	    (* Do not propagate over overwrite
		     * So if at entry of I, def X of a is possible, and I is:
		     * I: a = foo() -> J
		     * then at J def X is no longer possible
		     * (or, if X=I we already propagated in anyway)
		     *)
		    match (s#statement) with
		    |(_,`FCOMP(t,_),_)
			    -> if (t:>any_temp) <> temp then reachable_at_exit_of ssa_temp s
		    |(_,`VCOMP(t,_),_)
		    	    -> if (t:>any_temp) <> temp then reachable_at_exit_of ssa_temp s
		    |(_,`CCOMP(t,_),_)
		    	    -> if (t:>any_temp) <> temp then reachable_at_exit_of ssa_temp s
		    |(_,`MCOMP(t,_),_)
		    	    -> if (t:>any_temp) <> temp then reachable_at_exit_of ssa_temp s
		    |(_,`SCOMP(t,_),_)
		    	    -> if (t:>any_temp) <> temp then reachable_at_exit_of ssa_temp s
            	    | _     -> reachable_at_exit_of ssa_temp s
		)
	    and reachable_at_exit_of ssa_temp (s:statement) =
		mht_add reachability_at_exit (s#i) ssa_temp;
		if s#is_epilogue
		then
		    ()
		    (* failwith "Internal error, tried to exit the epilogue" *)
		else
		(
		    s#iterate_edges (fun edge -> reachable_at_entry_of ssa_temp (edge#dst_statement))
		)
	    in
	    let mark_phi (s:statement) =
		if ht_mem phi s#i
		then ()
		else
		(
		    (*printf "Phi of %s inserted at %d\n" (Print_ast2.any_temp_to_string temp) (s#i);*)
		    needs_a_rerun := true;
		    let new_temp = state#new_any_temp typ in
		    da_push definitions (false, s);
		    ht_set new_definition_temps (false, s) new_temp;
		    ht_set phi (s#i) new_temp
		)
	    in
	    da_iter (fun (is_normal_def, def_point) ->
		set_clear visited;
		let ssa_temp = ht_get new_definition_temps (is_normal_def, def_point) in
		if is_normal_def
		then reachable_at_exit_of ssa_temp def_point
		else reachable_inside_of  ssa_temp def_point
	    ) definitions;
	    (* Now we want to know where to insert PHI
	     * if we have [ A ] -> [ B ], and A.exit has only one reachable definition of x
	     * while B entry has more than one reachable definition of x, then insert phi at B
	     *)
	    mht_iter_all (fun i defs ->
		let s = new statement state i in
		if (List.length defs) = 1 (* Inside some definition's dominated area *)
	        then s#iterate_edges (fun edge ->
		    (*printf "Needs a phi at %s ?\n" edge#to_s;*)
		    if List.length (mht_get_all reachability_at_entry (edge#dst)) <> 1
		    then (*printf "    Yes\n";*) mark_phi (edge#dst_statement)
		    else () (*printf "    Not\n";*)
		)
	    ) reachability_at_exit;
	    if !needs_a_rerun
	    then
	    (
		(* printf "Rerun!\n"; *)
		iteration ()
	    )
	    else
	    (
		(*
		printf "Final reachability analysis of %s\n" (Print_ast2.any_temp_to_string temp);
		List.iter
		(fun i ->
		    let re = mht_find_all reachability_at_entry i in
		    let ri = mht_find_all reachability_inside   i in
		    let rx = mht_find_all reachability_at_exit  i in
		    printf "  %d: %s|%s|%s\n"
			i
			(match re with [] -> "?" | [temp] -> (Print_ast2.any_temp_to_string temp)
			                         | temps -> sprintf "<%s>" (list_join ";" (List.map Print_ast2.any_temp_to_string temps)))
			(match ri with [] -> "?" | [temp] -> (Print_ast2.any_temp_to_string temp)
			                         | temps -> sprintf "<%s>" (list_join ";" (List.map Print_ast2.any_temp_to_string temps)))
			(match rx with [] -> "?" | [temp] -> (Print_ast2.any_temp_to_string temp)
			                         | temps -> sprintf "<%s>" (list_join ";" (List.map Print_ast2.any_temp_to_string temps)))
		)
		(list_union (List.map hashtbl_keys
		    [reachability_at_entry; reachability_inside; reachability_at_exit]
		))
		;
		printf "\n";
		*)
		(reachability_inside, reachability_at_exit)
	    )
	in
	    let (reachability_inside, reachability_at_exit) = iteration () in
	    (* Now iterate over everything, changing the uses of the old
	     * definitions to the uses of the new definitions *)
	    mht_iter1 (fun i new_temp ->
		    (new statement state i)#op_temp_use_replace temp new_temp
	    ) reachability_inside;
	    (*
	    da_iter (fun (is_normal_def, def_point) ->
		let ssa_temp = ht_get new_definition_temps (is_normal_def, def_point) in
		if is_normal_def
		then printf "Normal def %s at %d\n" (Print_ast2.any_temp_to_string ssa_temp) def_point
		else printf "Phi-def %s at %d\n" (Print_ast2.any_temp_to_string ssa_temp) def_point
	    ) definitions;
	    *)
            (* And finally, replace the old definitions by the new ones *)
	    da_iter (fun (is_normal_def, def_point) ->
		(*printf "Running temp_wb_replace/add_phi_out for <%s,%d>\n" (if is_normal_def then "normal" else "phi") def_point#i;*)
		let ssa_temp = ht_get new_definition_temps (is_normal_def, def_point) in
		if is_normal_def
		then
		    def_point#temp_wb_replace temp ssa_temp
		else
		(
		    (*printf "New phi of %s at %d:\n" (Print_ast2.any_temp_to_string ssa_temp) def_point#i;*)
		    def_point#add_phi_in ssa_temp;
		    let phi_inputs = List.map
			(fun e ->
			    (e, try Some (mht_get_unique reachability_at_exit (e#src))
			        with Not_found -> None
			        |    Not_unique -> failwith "Internal error: reachability_at_exit not unique in SSA")
			) (mht_get_all inputs (def_point#i)) in
		    List.iter (fun (edge, t) ->
			match t with
			| Some(t) ->
			    (*printf "  in from %d is %s\n" i (Print_ast2.any_temp_to_string t); *)
			    edge#add_phi_out t
			| None ->
			    failwith (sprintf "Internal error: phi-in on %s is undefined" edge#to_s);
		    ) phi_inputs
		)
	    ) definitions;
    ) temps_that_need_ssa
end

let convert_to_ssa state =
    try convert_to_ssa_real state
    with Not_found -> failwith "Internal error: Not_found exception raised during SSA computations"
