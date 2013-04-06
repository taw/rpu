(***********************************************)
(* This file contains functions that operate   *)
(* on the compiler state                       *)
(***********************************************)

open Printf
open Ast2
open Util

(***********************************************)
(* Type aliases for convenience                *)
(***********************************************)

type semantic_info = [
| `MAGIC  of string
| `LOCAL
| `PARAM  of int
]

(***********************************************)
(***********************************************)
(***********************************************)
(* State type                                  *)
(***********************************************)
(***********************************************)
(***********************************************)

class compiler_state = 
object(self)
    val mutable label_counter = 3;
    val mutable temp_counter  = 0;

    (* Function start at preinit, preinit block initializes all local variables
     * and return value variable to 0 (float 0 or vector(0,0,0) or color(0,0,0)
     * or matrix 0 or ""), and jumps to codestart.
     * All return statements and the final "end out of the function block"
     * statements are converted to jumps to codeend
     *)
    
    (* FIXME: should call self#methods, but OCaml doesn't let us *)
    (* FIXME: This gives a new meaning to the horrible *)
    val mutable preinit_cpt  : codepoint = Obj.magic ()
    val mutable preinit_thr  : thread    = Obj.magic ()
    val mutable codestart_cpt: codepoint = Obj.magic ()
    val mutable codeend_cpt  : codepoint = Obj.magic ()
    
    method preinit_cpt   : codepoint
	= preinit_cpt
    method set_preinit_cpt : codepoint -> unit
    = fun cpt ->
	preinit_cpt <- cpt
    method codestart_thr : thread
	= new thread (self :> compiler_state) (codestart_cpt#i)
    method codeend_cpt   : codepoint
	= codeend_cpt

    val stmts : (int, Ast2.statement_cnt) ht = ht_new ()
    method stmts = stmts
    
    (* is_extern, is_array, basic_type, sem_info, temp *)
    val variables  : (string, (bool * bool * Ast2.var_typespec * semantic_info * any_temp)) ht = ht_new ()
    method variables = variables

    (* FIXME: That's seriously wrong design, it's not possible to simply kill them *)
    val params : any_temp da = da_new ();
    method params = params
    
    val magic_and_extern : prelude_element da = da_new ();
    method magic_and_extern = magic_and_extern

    val mutable epilogue_phi_in : any_temp list = []
    method epilogue_phi_in : any_temp list = epilogue_phi_in

    val mutable return : any_temp option = None;
    method return = return
    
    val writeback : epilogue_element da = da_new ();
    method writeback = writeback
    
    method extract_statements = ht_dup stmts
    
    method new_codepoint : codepoint = 
	let l = label_counter in
        (label_counter <- l+1;
	new codepoint (self:>compiler_state) l)

    method new_thread : thread =
	let l = label_counter in
        (label_counter <- l+1;
	new thread (self:>compiler_state) l)

    method variable_get_associated_temp : string -> any_temp
	= fun nam ->
	let (_, _, _, _, associated_temp) = ht_get variables nam in
	associated_temp

    method register cpt value =
	    ht_set_unique_or stmts cpt value
		(fun () -> failwith "Internal error: more than one statement with the same index")

    method var_typeinfo : string -> (bool * Ast2.var_typespec)
    = fun s ->
	try
	    let (_,is_array,base_type,_,_) = ht_get variables s in (is_array,base_type)
	with
	    Not_found -> (failwith (sprintf "Input error: Variable %s not known" s))

    method new_flt_temp : flt_temp
	    = let l = temp_counter in (temp_counter <- l+1; `FLT_TEMP(l))
    method new_vec_temp : (vec_type -> vec_temp)
	    = fun vti -> let l = temp_counter in (temp_counter <- l+1; `VEC_TEMP(l,vti))
    method new_col_temp : col_temp
	    = let l = temp_counter in (temp_counter <- l+1; `COL_TEMP(l))
    method new_mtx_temp : mtx_temp
	    = let l = temp_counter in (temp_counter <- l+1; `MTX_TEMP(l))
    method new_str_temp : str_temp
	    = let l = temp_counter in (temp_counter <- l+1; `STR_TEMP(l))
    method new_any_temp : var_typespec -> any_temp = fun ti ->
	match ti with
	| `FLOAT    -> ((self#new_flt_temp) :> any_temp)
	| `VEC(vti) -> ((self#new_vec_temp vti) :> any_temp)
	| `COLOR    -> ((self#new_col_temp) :> any_temp)
    | `MATRIX   -> ((self#new_mtx_temp) :> any_temp)
    | `STRING   -> ((self#new_str_temp) :> any_temp)

    (* FIXME: Do something about the sem_info *)
    method add_variable : string -> bool -> bool -> Ast2.var_typespec -> semantic_info -> any_temp
    = fun var_name is_extern is_array var_type sem_info ->
    (
    if is_array
    then
	failwith "FIXME: Arrays not supported yet"
    else
	let associated_temp = self#new_any_temp var_type in
	(
	    ht_set_unique_or
	    	variables
	    	var_name
	    	(is_extern, is_array, var_type, sem_info, associated_temp)
			(fun () -> failwith (sprintf "Input error: Variable %s defined more than once" var_name))
	);
	associated_temp
    )

    (* FIXME: add modification propagation
	      (like, s/t modified in a shader and then used in a function via extern) *)
    method add_magic_variable : string -> Ast2.var_typespec -> unit
    = fun var_name var_type ->
	let temp = self#add_variable var_name false false var_type (`MAGIC var_name)
        in da_push magic_and_extern (`MAGIC(var_name, temp))

    method add_var_preinit : any_temp -> unit
    = function
    | #flt_temp as t ->
	preinit_thr#register_statement(`FCOMP(t,`F_CONST(0.0)))
    | #vec_temp as t ->
	let zero_temp = preinit_thr#flt_comp(`F_CONST(0.0)) in
	preinit_thr#register_statement(`VCOMP(t,`V_FLTCAST(zero_temp)))
    | #col_temp as t ->
        let zero_temp = preinit_thr#flt_comp(`F_CONST(0.0)) in
	preinit_thr#register_statement(`CCOMP(t,`C_FLTCAST(zero_temp)))
    | #mtx_temp as t ->
        let zero_temp = preinit_thr#flt_comp(`F_CONST(0.0)) in
        preinit_thr#register_statement(`MCOMP(t,`M_FLTCAST(zero_temp)))
    | #str_temp as t ->
	preinit_thr#register_statement(`SCOMP(t,`S_CONST("")))

    method add_magic_out_variable : string -> Ast2.var_typespec -> unit
    = fun var_name var_type ->
	let temp = self#add_variable var_name false false var_type (`MAGIC var_name)
        in
	(
	    da_push magic_and_extern (`MAGIC(var_name, temp));
	    (* self#add_var_preinit temp; *) (* codegen is responsible for that *)
	    da_push writeback (`MAGIC(var_name, temp))
	)

    method add_local_variable : string -> (bool * Ast2.var_typespec) -> unit
    = fun name (is_extern, base_type) ->
	let temp = self#add_variable name is_extern false base_type `LOCAL
	in self#add_var_preinit temp (* Ignored *)

    (* FIXME: compile params *)
    method register_param : int -> Ast.param -> unit
    = fun i (`FORMAL(_,ti,nam,_)) ->
    (
	let temp = self#add_variable nam false false ti (`PARAM i) in
        da_push params temp
    )

    method register_params : Ast.param list -> unit
    = fun params ->
	list_iteri (self#register_param) params
	
    (***********************************************)
    (* State constructor for State superclass      *)
    (***********************************************)
    (* REFACTORME: Well, it would be nice to refactor it to use constructors *)
    method superclass_init =
    (
	label_counter <- 0;
	temp_counter  <- 0;

	preinit_cpt   <- self#new_codepoint;
	preinit_thr   <- preinit_cpt#start_thread;
    codestart_cpt <- self#new_codepoint;
    codeend_cpt   <- self#new_codepoint;

	ht_clear stmts;
	ht_clear variables;
	da_clear params;
	da_clear magic_and_extern;
	return <- None;
	epilogue_phi_in <- [];

	self#add_magic_variable "PI" (*`UNIFORM*) `FLOAT; (* PI is not magic, but it is in the standard library *)
    )

    (* TODO:  feed the state.variables pre-defined variables depending on the shader type
     * FIXME: the currently fed variable types are mostly random
     * TODO:  add semantic information to the variables table so we don't have to
     *        care about variable names at all, just compile special variables
     *        to `MAGIC and normal to `TEMP
     *)

    (***********************************************)
    (* State constructor for FunctionState subclass*)
    (***********************************************)
    method init_function : Ast.function_return_type -> unit
    = fun rt ->
    (
	self#superclass_init;
	match rt with
        | `VOID -> ()
	| (#var_typespec as rt) ->
        (
    	    let temp = self#new_any_temp rt in
	    return <- Some(temp);
	    self#add_var_preinit temp
        )
    )

    (***********************************************)
    (* State constructor for ShaderState subclass  *)
    (***********************************************)
    method init_shader : Ast.shader_type -> unit
    = fun st ->
    (
	self#superclass_init;
	match st with
        | `SURFACE ->
	    (
	    self#add_magic_variable "Cs"   (*`VARYING*) `COLOR;
	    self#add_magic_variable "Os"   (*`VARYING*) `COLOR;
	    self#add_magic_variable "P"    (*`VARYING*) (`VEC `POINT);

	    (* for triangles they're simply B-A and C-A vectors *)
	    self#add_magic_variable "dPdu" (*`VARYING*) (`VEC `VECTOR);
	    self#add_magic_variable "dPdv" (*`VARYING*) (`VEC `VECTOR);

	    self#add_magic_variable "N"    (*`VARYING*) (`VEC `NORMAL);
	    self#add_magic_variable "Ng"   (*`VARYING*) (`VEC `NORMAL);

	    self#add_magic_variable "u"    (*`VARYING*) `FLOAT;
	    self#add_magic_variable "v"    (*`VARYING*) `FLOAT;
	    self#add_magic_variable "du"   (*`VARYING*) `FLOAT;
	    self#add_magic_variable "dv"   (*`VARYING*) `FLOAT;
	    self#add_magic_variable "s"    (*`VARYING*) `FLOAT;
	    self#add_magic_variable "t"    (*`VARYING*) `FLOAT;
	    
	    (* FIXME: L, Cl, Ol ONLY INSIDE ILLUMINANCE STATEMENTS *)
	    self#add_magic_variable "L"    (*`VARYING*) (`VEC `VECTOR);
	    self#add_magic_variable "Cl"   (*`VARYING*) `COLOR;
	    self#add_magic_variable "Ol"   (*`VARYING*) `COLOR;

	    self#add_magic_variable "E"    (*`UNIFORM*) (`VEC `POINT);
	    self#add_magic_variable "I"    (*`VARYING*) (`VEC `VECTOR);

	    (* TODO: add ncomps time dtime dPdtime *)
	    self#add_magic_out_variable "Ci" (*`VARYING*) `COLOR;
	    self#add_magic_out_variable "Oi" (*`VARYING*) `COLOR;
	)
	| _ -> () (* FIXME *)
    )
    (* FIXME: This statement is the wrong (low-level, Ast2) statement *)
    (*
    method get_stmt : int -> statement
    = fun i ->
	    try ht_get stmts i
        with Not_found -> failwith (sprintf "Internal error: Invalid stmt index %d" i)
    *)

    method epilogue_temp_use_replace : any_temp -> any_temp -> unit
    = fun old_temp new_temp ->
    (
	return <- (match return with
                  | None -> None
		  | Some t -> Some(if t = old_temp then new_temp else t))
	;
	da_imperative_map (fun x -> match x with
	|`MAGIC(nam,temp)  -> `MAGIC(nam,(if temp = old_temp then new_temp else temp))
        |`EXTERN(nam,temp) -> `EXTERN(nam,(if temp = old_temp then new_temp else temp))
        |`OUTPUT(nam,temp) -> `OUTPUT(nam,(if temp = old_temp then new_temp else temp))
	) writeback;
    )
    
    method iterate_statements : (statement -> unit) -> unit
    = fun f ->
	ht_iter (fun k _ ->
	    f(new statement (self:>compiler_state) k)
	) stmts
	
    method prelude_edge = new edge_normal (self:>compiler_state) (-1) (preinit_cpt#i)

    (* ASSUMPTION: Return temp of the same type as the argument you got *)
    method global_temp_use_replace : (any_temp -> any_temp) -> unit
    = fun f ->
    (
	let ftf : flt_temp -> flt_temp = fun x -> (Obj.magic (f (Obj.magic x))) in
	let vtf : vec_temp -> vec_temp = fun x -> (Obj.magic (f (Obj.magic x))) in
	let ctf : col_temp -> col_temp = fun x -> (Obj.magic (f (Obj.magic x))) in
	let mtf : mtx_temp -> mtx_temp = fun x -> (Obj.magic (f (Obj.magic x))) in
	let stf : str_temp -> str_temp = fun x -> (Obj.magic (f (Obj.magic x))) in
	let fcf : flt_comp -> flt_comp = function
	|`F_CONST(f)        -> `F_CONST(f)
	|`F_ASG(src)        -> `F_ASG(ftf src)
	|`F_FUN(nam,args)   -> `F_FUN(nam, List.map f args)
	|`F_ARR_LD(nam,idx) -> `F_ARR_LD(nam, ftf idx)
	|`F_ADD(a,b)        -> `F_ADD(ftf a, ftf b)
	|`F_SUB(a,b)        -> `F_SUB(ftf a, ftf b)
	|`F_MUL(a,b)        -> `F_MUL(ftf a, ftf b)
	|`F_RCP(a)          -> `F_RCP(ftf a)
	|`F_RSQ(a)          -> `F_RSQ(ftf a)
	|`F_DOT_VV(a,b)     -> `F_DOT_VV(vtf a, vtf b)
	|`F_DOT_CC(a,b)     -> `F_DOT_CC(ctf a, ctf b)
	in
	let vcf : vec_comp -> vec_comp = function
	|`V_TUPLE(t)        -> `V_TUPLE(List.map ftf t)
	|`V_ASG(src)        -> `V_ASG(vtf src)
	|`V_FUN(nam,args)   -> `V_FUN(nam, List.map f args)
	|`V_ARR_LD(nam,idx) -> `V_ARR_LD(nam, ftf idx)
	|`V_FLTCAST(f)      -> `V_FLTCAST(ftf f)
	|`V_ADD(a,b)        -> `V_ADD(vtf a, vtf b)
	|`V_SUB(a,b)        -> `V_SUB(vtf a, vtf b)
	|`V_MUL(a,b)        -> `V_MUL(vtf a, vtf b)
	|`V_XPD(a,b)        -> `V_XPD(vtf a, vtf b)
	in
	let ccf : col_comp -> col_comp = function
	|`C_TUPLE(t)        -> `C_TUPLE(List.map ftf t)
	|`C_ASG(src)        -> `C_ASG(ctf src)
	|`C_FUN(nam,args)   -> `C_FUN(nam, List.map f args)
	|`C_ARR_LD(nam,idx) -> `C_ARR_LD(nam, ftf idx)
	|`C_FLTCAST(f)      -> `C_FLTCAST(ftf f)
	|`C_ADD(a,b)        -> `C_ADD(ctf a, ctf b)
	|`C_SUB(a,b)        -> `C_SUB(ctf a, ctf b)
	|`C_MUL(a,b)        -> `C_MUL(ctf a, ctf b)
	in
	let mcf : mtx_comp -> mtx_comp = function
	|`M_TUPLE(vs)       -> `M_TUPLE(List.map ftf vs)
	|`M_ASG(src)        -> `M_ASG(mtf src)
	|`M_FUN(nam, args)  -> `M_FUN(nam, List.map f args)
	|`M_ARR_LD(nam,idx) -> `M_ARR_LD(nam, ftf idx)
	|`M_FLTCAST(f)      -> `M_FLTCAST(ftf f)
	|`M_ADD(a,b)        -> `M_ADD(mtf a, mtf b)
	|`M_SUB(a,b)        -> `M_SUB(mtf a, mtf b)
	|`M_MUL(a,b)        -> `M_MUL(mtf a, mtf b)
	in
	let scf : str_comp -> str_comp = function
	|`S_CONST(s)      ->`S_CONST(s)
	|`S_ASG(src)      ->`S_ASG(stf src)
	|`S_FUN(nam,args) ->`S_FUN(nam,List.map f args)
	|`S_ARR_LD(s,i)   ->`S_ARR_LD(s,ftf i)
	in
	let cdf : cond -> cond = function
	|`GT(a,b)  -> `GT(ftf a, ftf b)
	|`GE(a,b)  -> `GE(ftf a, ftf b)
	|`FEQ(a,b) -> `FEQ(ftf a, ftf b)
	|`VEQ(a,b) -> `VEQ(vtf a, vtf b)
	|`CEQ(a,b) -> `CEQ(ctf a, ctf b)
	|`MEQ(a,b) -> `MEQ(mtf a, mtf b)
	|`SEQ(a,b) -> `SEQ(stf a, stf b)
	in
	
	ht_iter (fun i stmt ->
	    let (phi_in, op, (target_out, phi_out)) = stmt in
	    let new_phi_out = List.map f phi_out in
	    let new_op = match op with
	    | `NOP -> `NOP
	    | `CJMP(cond, (target_out_then, phi_out_then)) -> `CJMP(cdf cond, (target_out_then, List.map f phi_out_then))
	    | `FCOMP(dst, comp) -> `FCOMP(dst, fcf comp)
	    | `VCOMP(dst, comp) -> `VCOMP(dst, vcf comp)
	    | `CCOMP(dst, comp) -> `CCOMP(dst, ccf comp)
	    | `MCOMP(dst, comp) -> `MCOMP(dst, mcf comp)
	    | `SCOMP(dst, comp) -> `SCOMP(dst, scf comp)
	    | `PROCCALL(fc,args)-> `PROCCALL(fc, List.map f args)
	    in	
	    ht_set stmts i (phi_in, new_op, (target_out, new_phi_out))
	) stmts;
	return <- (match return with
	          | None      -> None
		  | Some temp -> Some (f temp))
	;
	da_imperative_map (function
	|`MAGIC(nam, temp)  ->`MAGIC(nam, f temp) 
	|`OUTPUT(nam, temp) ->`OUTPUT(nam, f temp)
	|`EXTERN(nam, temp) ->`EXTERN(nam, f temp) 
	) writeback;
    )

    (* run after expand_ast *)
    (*
     * At this point the function looks like:
     *
     * preinit_cpt:
     *   variable initializations
     * preinit_thr:
     *   EMPTY
     * codestart_cpt:
     *   the code
     * exit_thr:
     *   EMPTY
     * codeend_cpt:
     *
     * We need to add a jump from preinit to the new codestart,
     * and from exit_label to codeend.
     *
     * This is somewhat ugly. Maybe some refactoring can make it unnecessary.
     *)
    method finalize : thread -> unit
    = fun exit_thr ->
    (
	preinit_thr#register_jmp_and_die codestart_cpt;
        exit_thr#register_jmp_and_die codeend_cpt;
    )
    
    method epilogue_add_phi_in : any_temp -> unit =
    fun t ->
	epilogue_phi_in <- t::epilogue_phi_in
end
and codepoint (state:compiler_state)  (i:int) =
object
    method i = i
    method start_thread = new thread state i

    method register_cjmp : (Ast2.cond * codepoint * codepoint) -> unit
    = fun (cond,  then_cpt, else_cpt) ->
	state#register (i) ([], `CJMP(cond, ((then_cpt#i), [])), ((else_cpt#i), []))

    method replace_stmt : Ast2.statement_cnt -> unit
    = fun stmt ->
	ht_set state#stmts i stmt

    method remove_stmt : unit =
	ht_del state#stmts i
end
and thread (state:compiler_state) (i:int) =
object(self)
    val mutable i = i
    val mutable dead = false (* Only for internally-finished threads *)
    val mutable dev_null = false (* For unreachable code *)

    method codepoint : codepoint = new codepoint state i
    method forward : unit = (i <- state#new_codepoint#i)
(*
    method state : compiler_state = state
*)
    
    method die =
	dead <- true
    method go_to_dev_null =
	dev_null <- true
    method dev_null = dev_null

    (*
    method revive : codepoint -> unit
    = fun cpt -> 
	i    <- cpt#i;
	dead <- false
    *)

    (* Restart a thread, must be followed by some joins *)
    method restart : unit =
	if dev_null
	then
	    ()
	else if dead
	then
	(
	    i    <- state#new_codepoint#i;
	    dead <- false
	)
	else
	    failwith "Internal error: Trying to restart a thread that wasn't dead or in void"

    (* The thread is in dead-code area afterwards, but it's legal user input *)
    method register_return : unit =
	self#register_jmp_and_dev_null (state#codeend_cpt)

    (* The thread is in dead-code area afterwards, but it's legal user input *)
    method register_jmp_and_dev_null : codepoint -> unit
    = fun cpt ->
	if dev_null
	then
	    ()
	else
	(
	    state#register (self#codepoint#i) ([], `NOP, (cpt#i, []));
	    self#go_to_dev_null
	)

    method register_jmp_and_die : codepoint -> unit
    = fun cpt ->
	if dev_null
	then
	    ()
	else
	    state#register (self#codepoint#i) ([], `NOP, (cpt#i, []))
	;
	self#die

    method register_jmp_and_move : codepoint -> unit
    = fun cpt ->
	state#register (self#codepoint#i) ([], `NOP, (cpt#i, []));
	i <- cpt#i

(*    
    method register_cjmp_and_die : (cond * codepoint * codepoint) -> unit
    = fun (cond, then_cpt, else_cpt) ->
	self#codepoint#register_cjmp(cond, then_cpt, else_cpt);
	self#die
*)
    method conditional_join_and_die : (cond * thread * thread) -> unit
    = fun (cond, then_thr, else_thr) ->
	if dev_null
	then
	    ()
	else
	(
	    self#codepoint#register_cjmp(cond, then_thr#codepoint, else_thr#codepoint);
	    self#die
	)

    method join_and_die : thread -> unit
    = fun thr ->
	if dev_null
	then
	    ()
	else
	    self#register_jmp_and_die(thr#codepoint)

    method join_and_dev_null : thread -> unit
    = fun thr ->
	if dev_null
	then
	    ()
	else
	(
	    state#register (self#codepoint#i) ([], `NOP, (thr#codepoint#i, []));
	    self#go_to_dev_null
	)

    method register_statement : Ast2.op -> unit
	= fun op ->
	if dead
	then
	    failwith "Internal error: statements added to a thread after it has been killed"
	else if dev_null
	then
	    ()
	else
	(
	    let entry_cpt = i in
	    self#forward;
	    let exit_cpt = i in
    	    state#register (entry_cpt) ([], op, (exit_cpt, []))
	)
    
    method forward_cpts : (codepoint * codepoint) =
	let start_cpt = self#codepoint in
	self#forward;
	let end_cpt = self#codepoint in
	(start_cpt, end_cpt)
    
    method flt_comp : flt_comp -> [>flt_temp]
    = fun a ->
	let t = state#new_flt_temp in
	self#register_statement (`FCOMP(t,a));
	t

    method vec_comp : (vec_comp * vec_type) -> [>vec_temp]
    = fun (a, vti) ->
	let t = state#new_vec_temp vti in
	self#register_statement (`VCOMP(t,a));
	t

    method col_comp : col_comp -> [>col_temp]
    = fun a ->
	let t = state#new_col_temp in
	self#register_statement (`CCOMP(t,a));
	t

    method mtx_comp : mtx_comp -> [>mtx_temp]
    = fun a ->
	let t = state#new_mtx_temp in
	self#register_statement (`MCOMP(t,a));
	t

    method str_comp : str_comp -> [>str_temp]
    = fun a ->
	let t = state#new_str_temp in
	self#register_statement (`SCOMP(t,a));
	t

    method col_fltcast : flt_temp -> col_temp
    = fun t ->
	self#col_comp (`C_FLTCAST(t))

    (* Forwards to state, so we don't have to expose state directly
     * It would be really nice to have delegate keyword
     *)
    method variable_get_associated_temp : string -> any_temp
    = fun var_name ->
	state#variable_get_associated_temp var_name

    method var_typeinfo : string -> (bool * Ast2.var_typespec)
    = fun var_name ->
	state#var_typeinfo var_name

    method new_any_temp : var_typespec -> any_temp
    = fun ti ->
	state#new_any_temp ti

    method add_local_variable : string -> (bool * Ast2.var_typespec) -> unit
    = fun nam typ ->
	state#add_local_variable nam typ
    
    method return : any_temp option = state#return
    
    (* TODO: new_thread/new_codepoint methods should only return a valid
     * thread/codepoint if the current thread is not dev_null.
     * If it is dev_null, they should return a dev_null thread/codepoint
     *)
    method fork_thread : thread =
	if dev_null
	then
	    (self:>thread) (* FIXME: This is probably totally wrong *)
	else
	    state#new_thread
    (*
    method new_codepoint : codepoint = state#new_codepoint
    *)
end

(***********************************************)
(* The new object framework for edges and nodes*)
(***********************************************)

(* All are subclasses of edge, should have identical interface *)
and virtual edge =
object(self)
    method virtual state : compiler_state
    method virtual src : int
    method virtual dst : int
    method virtual add_phi_out : any_temp -> unit
    method virtual set_phi_out : any_temp list -> unit
    method virtual to_s : string

    method src_statement = new statement (self#state) (self#src)
    method dst_statement = new statement (self#state) (self#dst)

    method virtual phi_out: any_temp list


    method phi_in : any_temp list =
	if self#dst_statement#is_prelude
	then failwith "Internal error: edge#phi_in called on edge to prelude"
	else if self#dst_statement#is_epilogue
	then self#state#epilogue_phi_in
	else
	    let (phi_in, _, _) = self#dst_statement#statement in phi_in
end
(* This should support normal, from-prelude and to-epilogue edges 
   (maybe even from-prelude-to-epilogue edges) *)
and edge_normal (state:compiler_state) (i:int) (j:int) =
object(self)
    inherit edge
    method state = state
    method src = i
    method dst = j

    method phi_out =
	if self#src_statement#is_prelude
	then failwith "Internal error: edge#phi_out called on edge to prelude"
	else if self#src_statement#is_epilogue
	then failwith "Internal error: edge#phi_out called on edge to epilogue"
	else
	    let (_, _, (_, phi_out)) = self#src_statement#statement in phi_out

    method add_phi_out t =
	try
	    let src_statement = self#src_statement in
	    if src_statement#is_prelude
	    then
		failwith "FIXME: Phi-link from prelude not supported yet"
	    else if src_statement#is_epilogue
	    then
		failwith "Internal error: Phi-link from epilogue"
	    else	    
        	let (phi_in, op, (target_out, phi_out)) = src_statement#statement in
                src_statement#set_statement (phi_in, op, (target_out, t::phi_out))
	with Not_found -> failwith (sprintf "Internal error: Not_found during add_phi_out of %s" self#to_s)

    method set_phi_out new_phi_out =
	try
	    let src_statement = self#src_statement in
	    if src_statement#is_prelude
	    then
		failwith "FIXME: Phi-link from prelude not supported yet"
	    else if src_statement#is_epilogue
	    then
		failwith "Internal error: Phi-link from epilogue"
	    else	    
        	let (phi_in, op, (target_out, _)) = src_statement#statement in
                src_statement#set_statement (phi_in, op, (target_out, new_phi_out))
	with Not_found -> failwith (sprintf "Internal error: Not_found during new_phi_out of %s" self#to_s)

    method to_s = sprintf "edge from %d to %d" i j
end

and edge_then (state:compiler_state) (i:int) (j:int) =
object(self)
    inherit edge
    method state = state
    method src = i
    method dst = j

    method phi_out =
	if self#src_statement#is_prelude
	then failwith "Internal error: edge#phi_out called on edge to prelude"
	else if self#src_statement#is_epilogue
	then failwith "Internal error: edge#phi_out called on edge to epilogue"
	else
	    match self#src_statement#statement with
	    | (_, `CJMP(_, (_, then_phi_out)), _) -> then_phi_out
	    | _ -> failwith (sprintf "Internal error: Then-edge(%d->%d) not coming out of a CJMP instruction" i j)

    method add_phi_out t =
	    let src_statement = self#src_statement in
	    if src_statement#is_prelude || src_statement#is_epilogue
	    then
		failwith "Internal error: Only normal edges can be then-edges"
	    else
	        match src_statement#statement with
		| (phi_in, `CJMP(cond, (then_target_out, then_phi_out)), else_link_out) ->
            	    src_statement#set_statement (phi_in, `CJMP(cond, (then_target_out, t::then_phi_out)), else_link_out)
	        | _ -> failwith (sprintf "Internal error: Then-edge(%d->%d) not coming out of a CJMP instruction" i j)

    method set_phi_out new_phi_out =
	    let src_statement = self#src_statement in
	    if src_statement#is_prelude || src_statement#is_epilogue
	    then
		failwith "Internal error: Only normal edges can be then-edges"
	    else
	        match src_statement#statement with
		| (phi_in, `CJMP(cond, (then_target_out, _)), else_link_out) ->
            	    src_statement#set_statement (phi_in, `CJMP(cond, (then_target_out, new_phi_out)), else_link_out)
	        | _ -> failwith (sprintf "Internal error: Then-edge(%d->%d) not coming out of a CJMP instruction" i j)

    method to_s = sprintf "else-edge from %d to %d" i j
end

and edge_else (state:compiler_state) (i:int) (j:int) =
object(self)
    inherit edge
    method state = state
    method src = i
    method dst = j

    method phi_out =
	if self#src_statement#is_prelude
	then failwith "Internal error: edge#phi_out called on edge to prelude"
	else if self#src_statement#is_epilogue
	then failwith "Internal error: edge#phi_out called on edge to epilogue"
	else
	    let (_, _, (_, phi_out)) = self#src_statement#statement in phi_out

    method add_phi_out t =
	    let src_statement = self#src_statement in
	    if src_statement#is_prelude || src_statement#is_epilogue
	    then
		failwith "Internal error: Only normal edges can be else-edges"
	    else
        	let (phi_in, op, (else_target_out, else_phi_out)) = src_statement#statement in
            	    src_statement#set_statement (phi_in, op, (else_target_out, t::else_phi_out))

    method set_phi_out new_phi_out =
	    let src_statement = self#src_statement in
	    if src_statement#is_prelude || src_statement#is_epilogue
	    then
		failwith "Internal error: Only normal edges can be else-edges"
	    else
        	let (phi_in, op, (else_target_out, _)) = src_statement#statement in
            	    src_statement#set_statement (phi_in, op, (else_target_out, new_phi_out))

    method to_s = sprintf "then-edge from %d to %d" i j
end

(* TODO: wrap all methods that may fail on prelude/epilogue statements *
 * in nice tests+exceptions                                            *)
and statement (state:compiler_state) (i:int) =
object(self)
    method i = i
    method statement  = if self#is_prelude
                        then
			    failwith "Internal error: Trying to run #statement method on prelude"
			else if self#is_epilogue
			then
			    failwith "Internal error: Trying to run #statement method on epilogue"
			else
		    	    try ht_get (state#stmts) i
                    with Not_found -> failwith (sprintf "Internal error: Trying to run #statement method on invalid statement %d" i)
    (* ASSUMPTION: drop assumes the code is already unreachable
     *             Running it on reachable code may result it crashes
     *)
    method drop       = if self#is_prelude
                        then
			    failwith "Internal error: Trying to run #drop method on prelude"
			else if self#is_epilogue
			then
			    failwith "Internal error: Trying to run #drop method on epilogue"
			else
			    ht_del state#stmts i

    method set_statement s = ht_set (state#stmts) i s
    method op         = if self#is_prelude
                        then
			    failwith "Internal error: Trying to run #op method on prelude"
			else if self#is_epilogue
			then 
			    failwith "Internal error: Trying to run #op method on epilogue"
			else
			    match self#statement with (_, op, _) -> op
    method set_op op  = if self#is_prelude
                        then
			    failwith "Internal error: Trying to run #set_op method on prelude"
			else if self#is_epilogue
			then 
			    failwith "Internal error: Trying to run #set_op method on epilogue"
			else
                    	     match self#statement with (link_in, _, link_out) -> self#set_statement (link_in, op, link_out)
    method link       = if self#is_prelude
                        then
			    failwith "Internal error: Trying to run #link method on prelude"
			else if self#is_epilogue
			then 
			    failwith "Internal error: Trying to run #link method on epilogue"
			else
			    let (_, op, link) = self#statement in match op with
                    	    | `CJMP(_, _) -> failwith "Internal error: CJMP used as a single-out instruction"
		    	    | _ -> link
    (* Or make it return self#link instead of failing ? *)
    method link_t     = let (_, op, _) = self#statement in match op with
                      | `CJMP(_, link_t) -> link_t
		      | _ -> failwith (sprintf "Internal error: Instruction %d is not a CJMP, but its then-link is requested" i)
    method link_e     = let (_, op, link_e) = self#statement in match op with
                      | `CJMP(_, _) -> link_e
		      | _ -> failwith (sprintf "Internal error: Instruction %d is not a CJMP, but its else-link is requested" i)
    method target_t   = match self#link_t with (target_t, _) -> target_t
    method target_e   = match self#link_e with (target_e, _) -> target_e
    method target     = match self#link   with (target, _)   -> target
    method make_nop   = self#set_op (`NOP)
    
    method is_cjmp    = if self#is_prelude || self#is_epilogue
			then
			    false
			else
			    match self#op with
                    	    | `CJMP(_,_) -> true
		    	    | _ -> false
    method edge       = new edge_normal state i (self#target)
    method edge_t     = new edge_then   state i (self#target_t)
    method edge_e     = new edge_else   state i (self#target_e)
    method edges      = if self#is_prelude
                        then
			    [state#prelude_edge]
			else if self#is_epilogue
			then
			    []
			else if self#is_cjmp
                    	then
			    [self#edge_t; self#edge_e]
			else
			    [self#edge]
    method iterate_edges f =
			List.iter f self#edges

    method normal_defs = match self#op with
                       | `FCOMP(temp,_) -> [(temp :> any_temp)]
                       | `VCOMP(temp,_) -> [(temp :> any_temp)]
                       | `CCOMP(temp,_) -> [(temp :> any_temp)]
                       | `MCOMP(temp,_) -> [(temp :> any_temp)]
                       | `SCOMP(temp,_) -> [(temp :> any_temp)]
		       | _ -> []

    method iterate_normal_defs f =
			List.iter f self#normal_defs

    method add_phi_in t =
			if self#is_prelude
                        then
			    failwith "Internal error: Trying to run #add_phi_in method on prelude"
			else if self#is_epilogue
			then
			    state#epilogue_add_phi_in t
			else
                    	    let (phi_in, op, link_out) = self#statement in
                    	    self#set_statement (t::phi_in, op, link_out)

    method phi_in = let (phi_in, _, _) = self#statement in phi_in
    method set_phi_in new_phi_in =
			if self#is_prelude
                        then
			    failwith "Internal error: Trying to run #set_phi_in method on prelude"
			else if self#is_epilogue
			then
			    failwith "Internal error: Trying to run #set_phi_in method on epilogue"
			else
                    	    let (_, op, link_out) = self#statement in
                    	    self#set_statement (new_phi_in, op, link_out)
    

    method is_prelude  = (i = -1)
    method is_epilogue = (i = state#codeend_cpt#i)

    (* The type system's refusal to do the right subtyping is simply a huge PITA here.
     * Turn all the type checks off by Obj.magic.
     *
     * Damn, the type system refuses to STFU. Even if I explicitly type r as 'a->'a,
     * it converts this type to something else
     *)
     (* MAJOR FIXME: IT ONLY HANDLES TEMPs USED INSIDE OP, NOT ON PHI-OUT EDGES
      *)
    method op_temp_use_replace : any_temp -> any_temp -> unit
    = fun old_temp new_temp ->    
    let rv = fun t -> if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	let rf = fun t -> if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	let rc = fun t -> if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	let rm = fun t -> if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	let rs = fun t -> if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	let ra = fun t -> if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	let funcall_replace (nam, args) = (nam, List.map ra args)
	in
	let fcr = function
	|`F_CONST(f)        -> `F_CONST(f)
	|`F_ASG(src)        -> `F_ASG(rf src)
	|`F_FUN(fc)         -> `F_FUN(funcall_replace fc)
	|`F_ARR_LD(nam,idx) -> `F_ARR_LD(nam, rf idx)
	|`F_ADD(a,b)        -> `F_ADD(rf a, rf b)
	|`F_SUB(a,b)        -> `F_SUB(rf a, rf b)
	|`F_MUL(a,b)        -> `F_MUL(rf a, rf b)
	|`F_RCP(a)          -> `F_RCP(rf a)
	|`F_RSQ(a)          -> `F_RSQ(rf a)
	|`F_DOT_VV(a,b)     -> `F_DOT_VV(rv a, rv b)
	|`F_DOT_CC(a,b)     -> `F_DOT_CC(rv a, rv b)
	in
	let vcr = function
	|`V_TUPLE(t)        -> `V_TUPLE(List.map rf t)
	|`V_ASG(src)        -> `V_ASG(rv src)
	|`V_FUN(fc)         -> `V_FUN(funcall_replace fc)
	|`V_ARR_LD(nam,idx) -> `V_ARR_LD(nam, rf idx)
	|`V_FLTCAST(f)      -> `V_FLTCAST(rf f)
	|`V_ADD(a,b)        -> `V_ADD(rv a, rv b)
	|`V_SUB(a,b)        -> `V_SUB(rv a, rv b)
	|`V_MUL(a,b)        -> `V_MUL(rv a, rv b)
	|`V_XPD(a,b)        -> `V_XPD(rv a, rv b)
	in
	let ccr = function
	|`C_TUPLE(t)        -> `C_TUPLE(List.map rf t)
	|`C_ASG(src)        -> `C_ASG(rc src)
	|`C_FUN(fc)         -> `C_FUN(funcall_replace fc)
	|`C_ARR_LD(nam,idx) -> `C_ARR_LD(nam, rf idx)
	|`C_FLTCAST(f)      -> `C_FLTCAST(rf f)
	|`C_ADD(a,b)        -> `C_ADD(rc a, rc b)
	|`C_SUB(a,b)        -> `C_SUB(rc a, rc b)
	|`C_MUL(a,b)        -> `C_MUL(rc a, rc b)
	in
	let mcr = function
	|`M_TUPLE(t)        -> `M_TUPLE(List.map rf t)
	|`M_ASG(src)        -> `M_ASG(rm src)
	|`M_FUN(fc)         -> `M_FUN(funcall_replace fc)
	|`M_ARR_LD(nam,idx) -> `M_ARR_LD(nam, rf idx)
	|`M_FLTCAST(f)      -> `M_FLTCAST(rf f)
	|`M_ADD(a,b)        -> `M_ADD(rm a, rm b)
	|`M_SUB(a,b)        -> `M_SUB(rm a, rm b)
	|`M_MUL(a,b)        -> `M_MUL(rm a, rm b)
	in
	let scr = function
	|`S_CONST(str)      -> `S_CONST(str)
	|`S_ASG(src)        -> `S_ASG(rs src)
	|`S_FUN(fc)         -> `S_FUN(funcall_replace fc)
	|`S_ARR_LD(nam,idx) -> `S_ARR_LD(nam, rf idx)
	in
	let rcond = function
	|`GT(a,b)  -> `GT(rf a, rf b)
	|`GE(a,b)  -> `GE(rf a, rf b)
	|`FEQ(a,b) -> `FEQ(rf a, rf b)
	|`VEQ(a,b) -> `VEQ(rv a, rv b)
	|`CEQ(a,b) -> `CEQ(rc a, rc b)
	|`MEQ(a,b) -> `MEQ(rm a, rm b)
	|`SEQ(a,b) -> `SEQ(rs a, rs b)
	in
	let rop = function
	|`FCOMP(dst, comp) -> `FCOMP(dst, fcr comp)
	|`VCOMP(dst, comp) -> `VCOMP(dst, vcr comp)
	|`CCOMP(dst, comp) -> `CCOMP(dst, ccr comp)
	|`MCOMP(dst, comp) -> `MCOMP(dst, mcr comp)
	|`SCOMP(dst, comp) -> `SCOMP(dst, scr comp)
	|`NOP              -> `NOP
	|`CJMP(cd,si)      -> `CJMP(rcond cd, si)
	|`PROCCALL(fc)     -> `PROCCALL(funcall_replace fc)
	in
	if self#is_prelude
	then
	    failwith "Internal error: Tried to replace temp use in prelude"
	else if self#is_epilogue
	then
	    state#epilogue_temp_use_replace old_temp new_temp
	else
        (
	    let (phi_in,op,phi_out) = self#statement in
	    let new_op = rop op in
	    if op <> new_op then
	    self#set_statement (phi_in,new_op,phi_out)
	)

    (* MAJOR FIXME: ONLY OP-WRITE IS REPLACED, NOT PHI-IN *)
    method temp_wb_replace : any_temp -> any_temp -> unit
    = fun old_temp new_temp ->
	let rf t = if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	let rv t = if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	let rc t = if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	let rm t = if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	let rs t = if (t:>any_temp) = old_temp then Obj.magic new_temp else t in
	if self#is_prelude
	then
	(
	    da_imperative_map (fun temp ->
		if temp = old_temp then new_temp else temp
	    ) state#params;
	    da_imperative_map (function
		|`MAGIC(nam,temp)  -> `MAGIC(nam,(if temp = old_temp then new_temp else temp))
		|`EXTERN(nam,temp) -> `EXTERN(nam,(if temp = old_temp then new_temp else temp))
	    ) state#magic_and_extern;
	)
	else if self#is_epilogue
	then
	    failwith "FIXME: temp_wb_replace on epilogue" (* FIXME: replace-at-epilogue support *)
	else
	    let (phi_in,op,phi_out) = self#statement in
	    let new_op = match op with
	    |`FCOMP(dst, comp) -> `FCOMP(rf dst, comp)
	    |`VCOMP(dst, comp) -> `VCOMP(rv dst, comp)
	    |`CCOMP(dst, comp) -> `CCOMP(rc dst, comp)
            |`MCOMP(dst, comp) -> `MCOMP(rm dst, comp)
            |`SCOMP(dst, comp) -> `SCOMP(rs dst, comp)
            |`NOP              -> `NOP
            |`CJMP(cd,si)      -> `CJMP(cd,si)
            |`PROCCALL(fc)     -> `PROCCALL(fc)
	    in
            self#set_statement (phi_in, new_op, phi_out)
end
