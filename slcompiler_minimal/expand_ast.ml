open Printf
open Ast2
open Util
open Compiler_state

(* Main *)

(* ABI Registry *)
let abi_registry : (string, (fun_typespec * var_typespec list)) ht = ht_new ()

let function_abi_register (name, rt, params) =
(
    ht_set abi_registry name (rt, List.map (function `FORMAL(_,t,_,_) -> t) params)
)
let shader_abi_register (name, st) =
match st with
| `SURFACE -> ht_set abi_registry name (`COLOR, [`VEC(`POINT); `VEC(`VECTOR)])
| _        -> failwith "TODO: ABI of shaders other than surface shaders unknown"

(* TUPLE is temporary value only, it should not be present in the final output *)
type compiled_expr = [
| `TUPLE of tuple
| any_temp
]

(* NEW CODE GOES HERE *)
let compiled_expr_type_to_string : [<compiled_expr] -> string
= function
| `TUPLE(_)   -> "bare tuple"
| `FLT_TEMP(_) -> "float"
| `VEC_TEMP(_,`POINT)  -> "point"
| `VEC_TEMP(_,`VECTOR) -> "vector"
| `VEC_TEMP(_,`NORMAL) -> "normal"
| `COL_TEMP(_) -> "color"
| `MTX_TEMP(_) -> "matrix"
| `STR_TEMP(_) -> "string"

let type_error :
    (string -> string -> string, unit, string) format
    -> [<compiled_expr]
    -> [<compiled_expr]
    -> 'a
= fun fmt a b ->
    failwith (sprintf fmt (compiled_expr_type_to_string a) (compiled_expr_type_to_string b))

let type_error_1 :
    (string -> string, unit, string) format
    -> [<compiled_expr]
    -> 'a
= fun fmt a ->
    failwith (sprintf fmt (compiled_expr_type_to_string a))

let vtinfo_add : (vec_type * vec_type -> vec_type)
= function
| (`POINT,_)       -> `POINT
| (_,`POINT)       -> `POINT
| (`VECTOR,_)      -> `VECTOR
| (_,`VECTOR)      -> `VECTOR
| (`NORMAL,`NORMAL)-> `NORMAL
let vtinfo_sub = function
| (`POINT,`POINT)  -> `VECTOR
| (`POINT,_)       -> `POINT
| (_,`POINT)       -> `POINT
| (`VECTOR,_)      -> `VECTOR
| (_,`VECTOR)      -> `VECTOR
| (`NORMAL,`NORMAL)-> `NORMAL
let vtinfo_mul = function
| (`VECTOR,_)      -> `VECTOR
| (_,`VECTOR)      -> `VECTOR
| (`NORMAL,_)      -> `NORMAL
| (_,`NORMAL)      -> `NORMAL
| (`POINT,`POINT)  -> `POINT
let vtinfo_xpd = function
| (`VECTOR,`VECTOR)-> `NORMAL
| (_,_)            -> `VECTOR

let vti : vec_temp -> vec_type
= function (`VEC_TEMP(_,vti)) -> vti

let compile_add : (compiled_expr * compiled_expr * thread) -> compiled_expr
= fun (a, b, thr) ->
match (a, b) with
| (#flt_temp as a),(#flt_temp as b) ->
	(thr#flt_comp (`F_ADD(a,b)) :> compiled_expr)
| (#vec_temp as a),(#flt_temp as b) ->
	let avt = vti a in
	let b = thr#vec_comp (`V_FLTCAST(b),avt) in
	(thr#vec_comp (`V_ADD(a,b),avt) :> compiled_expr)
| (#flt_temp as a),(#vec_temp as b) ->
	let bvt = vti b in
	let a = thr#vec_comp (`V_FLTCAST(a),bvt) in
        (thr#vec_comp (`V_ADD(a,b),bvt) :> compiled_expr)
| (#vec_temp as a),(#vec_temp as b) ->
	(thr#vec_comp (`V_ADD(a,b),vtinfo_add(vti a,vti b)) :> compiled_expr)
| (#col_temp as a),(#col_temp as b) ->
	(thr#col_comp (`C_ADD(a,b)) :> compiled_expr)
| (#col_temp as a),(#flt_temp as b) ->
	let b = thr#col_comp (`C_FLTCAST(b)) in
	(thr#col_comp (`C_ADD(a,b)) :> compiled_expr)
| (#flt_temp as a),(#col_temp as b) ->
	let a = thr#col_comp (`C_FLTCAST(a)) in
	(thr#col_comp (`C_ADD(a,b)) :> compiled_expr)
| _ -> type_error "FIXME: Can't compile %s+%s (yet?)" a b

(* FIXME: It's kinda silly to cast point - float to point - point,
 *        as the result is point not vector
 *)
let compile_sub : (compiled_expr * compiled_expr * thread) -> compiled_expr
= fun (a, b, thr) ->
match (a, b) with
| (#flt_temp as a),(#flt_temp as b) ->
	(thr#flt_comp (`F_SUB(a,b)) :> compiled_expr)
| (#vec_temp as a),(#flt_temp as b) ->
	let avt = vti a in
	let b = thr#vec_comp (`V_FLTCAST(b),avt) in
	(thr#vec_comp (`V_SUB(a,b),avt) :> compiled_expr)
| (#flt_temp as a),(#vec_temp as b) ->
	let bvt = vti b in
	let a = thr#vec_comp (`V_FLTCAST(a),bvt) in
        (thr#vec_comp (`V_SUB(a,b),bvt) :> compiled_expr)
| (#vec_temp as a),(#vec_temp as b) ->
	(thr#vec_comp (`V_SUB(a,b),vtinfo_add(vti a,vti b)) :> compiled_expr)
| (#col_temp as a),(#col_temp as b) ->
	(thr#col_comp (`C_SUB(a,b)) :> compiled_expr)
| (#col_temp as a),(#flt_temp as b) ->
	let b = thr#col_comp (`C_FLTCAST(b)) in
	(thr#col_comp (`C_SUB(a,b)) :> compiled_expr)
| (#flt_temp as a),(#col_temp as b) ->
	let a = thr#col_comp (`C_FLTCAST(a)) in
	(thr#col_comp (`C_SUB(a,b)) :> compiled_expr)
| _ -> type_error "FIXME: Can't compile subtraction %s-%s (yet?)" a b

let compile_mul : (compiled_expr * compiled_expr * thread) -> compiled_expr
= fun (a, b, thr) ->
match (a, b) with
| (#flt_temp as a),(#flt_temp as b) ->
	(thr#flt_comp (`F_MUL(a,b)) :> compiled_expr)
| (#vec_temp as a),(#flt_temp as b) ->
	let avt = vti a in
	let b = thr#vec_comp (`V_FLTCAST(b),avt) in
	(thr#vec_comp (`V_MUL(a,b),avt) :> compiled_expr)
| (#flt_temp as a),(#vec_temp as b) ->
	let bvt = vti b in
	let a = thr#vec_comp (`V_FLTCAST(a),bvt) in
        (thr#vec_comp (`V_MUL(a,b),bvt) :> compiled_expr)
| (#vec_temp as a),(#vec_temp as b) ->
	(thr#vec_comp (`V_MUL(a,b),vtinfo_mul(vti a,vti b)) :> compiled_expr)
| (#col_temp as a),(#col_temp as b) ->
	(thr#col_comp (`C_MUL(a,b)) :> compiled_expr)
| (#col_temp as a),(#flt_temp as b) ->
	let b = thr#col_fltcast(b) in
	(thr#col_comp (`C_MUL(a,b)) :> compiled_expr)
| (#flt_temp as a),(#col_temp as b) ->
	let (a) = thr#col_fltcast(a) in
	(thr#col_comp (`C_MUL(a,b)) :> compiled_expr)
| _ -> type_error "FIXME: Can't compile multiplication %s*%s (yet?)" a b

let compile_div : (compiled_expr * compiled_expr * thread) -> compiled_expr
= fun (a, b, thr) ->
match (a, b) with
| (#flt_temp as a),(#flt_temp as b) ->
	let bi = thr#flt_comp (`F_RCP(b)) in
	(thr#flt_comp (`F_MUL(a,bi)) :> compiled_expr)
| (#vec_temp as a),(#flt_temp as b) ->
	let bi = thr#flt_comp (`F_RCP(b)) in
	let bi = thr#vec_comp (`V_FLTCAST(bi),vti a) in
	(thr#vec_comp (`V_MUL(a,bi),vti a) :> compiled_expr)
| (#col_temp as a),(#flt_temp as b) ->
	let bi = thr#flt_comp (`F_RCP(b)) in
	let bi = thr#col_fltcast(bi)  in
	(thr#col_comp (`C_MUL(a,bi)) :> compiled_expr)
| _ -> type_error "FIXME: Can't compile division %s/%s (yet?)" a b

let compile_dot : (compiled_expr * compiled_expr * thread) -> compiled_expr
= fun (a, b, thr) ->
match (a, b) with
| (#vec_temp as a),(#vec_temp as b) ->
	(thr#flt_comp (`F_DOT_VV(a,b)) :> compiled_expr)
| (#col_temp as a),(#col_temp as b) ->
	(thr#flt_comp (`F_DOT_CC(a,b)) :> compiled_expr)
| _ -> type_error "FIXME: Can't compile dot product %s.%s (yet?)" a b

let compile_xpd : (compiled_expr * compiled_expr * thread) -> compiled_expr
= fun (a, b, thr) ->
match (a, b) with
| (#vec_temp as a),(#vec_temp as b) ->
	(thr#vec_comp (`V_XPD(a,b),vtinfo_xpd(vti a,vti b)) :> compiled_expr)
| _ -> type_error "FIXME: Can't compile cross product %s^%s (yet?)" a b

let compile_var_ld : (string * thread) -> compiled_expr
= fun (nam, thr) ->
    let temp = thr#variable_get_associated_temp nam in
    match temp with
    |#flt_temp as t -> (thr#flt_comp (`F_ASG(t)) :> compiled_expr)
    |#vec_temp as t -> (thr#vec_comp (`V_ASG(t), vti t) :> compiled_expr)
    |#col_temp as t -> (thr#col_comp (`C_ASG(t)) :> compiled_expr)
    |#mtx_temp as t -> (thr#mtx_comp (`M_ASG(t)) :> compiled_expr)
    |#str_temp as t -> (thr#str_comp (`S_ASG(t)) :> compiled_expr)

let compile_temp_asg : (any_temp * compiled_expr * thread) -> unit
= fun (dst, src, thr) ->
match (dst, src) with
    | (#flt_temp as dst),(#flt_temp as src) ->
	thr#register_statement (`FCOMP(dst,`F_ASG(src)))
    | (#flt_temp as dst),src ->
	type_error "Can't compile assignment %s=%s" dst src

    | (#vec_temp as dst),(#vec_temp as src) ->
	thr#register_statement (`VCOMP(dst,`V_ASG(src)))
    | (#vec_temp as dst),(#flt_temp as src) ->
	let src = thr#vec_comp (`V_FLTCAST(src),vti dst) in
	thr#register_statement (`VCOMP(dst,`V_ASG(src)))
    | (#vec_temp as dst),(`TUPLE(vs)) ->
	let src = thr#vec_comp (`V_TUPLE(vs),vti dst) in
	thr#register_statement (`VCOMP(dst,`V_ASG(src)))
    | (#vec_temp as dst),src ->
	type_error "Can't compile assignment %s=%s" dst src

    | (#col_temp as dst),(#col_temp as src) ->
	thr#register_statement (`CCOMP(dst,`C_ASG(src)))
    | (#col_temp as dst),(#flt_temp as src) ->
	let src = thr#col_comp (`C_FLTCAST(src)) in
	thr#register_statement (`CCOMP(dst,`C_ASG(src)))
    | (#col_temp as dst),(`TUPLE(vs)) ->
	let src = thr#col_comp (`C_TUPLE(vs)) in
	thr#register_statement (`CCOMP(dst,`C_ASG(src)))
    | (#col_temp as dst),src ->
	type_error "Can't compile assignment %s=%s" dst src

    | (#mtx_temp as dst),(#mtx_temp as src) ->
	thr#register_statement (`MCOMP(dst,`M_ASG(src)))
    | (#mtx_temp as dst),(#flt_temp as src) ->
	let src = thr#mtx_comp (`M_FLTCAST(src)) in
	thr#register_statement (`MCOMP(dst,`M_ASG(src)))
    | (#mtx_temp as dst),(`TUPLE(vs)) ->
	let src = thr#mtx_comp (`M_TUPLE(vs)) in
	thr#register_statement (`MCOMP(dst,`M_ASG(src)))

    | (#mtx_temp as dst),src ->
	type_error "Can't compile assignment %s=%s" dst src

    | (#str_temp as dst),(#str_temp as src) ->
	thr#register_statement (`SCOMP(dst,`S_ASG(src)))

    | (#str_temp as dst),src ->
	type_error "Can't compile assignment %s=%s" dst src

let compile_var_st : (string * compiled_expr * thread) -> compiled_expr
= fun (nam, e, thr) ->
    let temp = thr#variable_get_associated_temp nam in
    compile_temp_asg(temp, e, thr);
    compile_var_ld(nam, thr)

let compile_asgop : (compiled_expr * Ast.asg_op * compiled_expr * thread) -> compiled_expr
= fun (v, op, e, thr) -> match op with
| `EQ      -> e
| `PLUSEQ  -> compile_add (v, e, thr)
| `MINUSEQ -> compile_sub (v, e, thr)
| `TIMESEQ -> compile_mul (v, e, thr)
| `DIVEQ   -> compile_div (v, e, thr)

let rec compile_expr : (Ast.expr * thread) -> compiled_expr
= fun (e, thr) -> match e with
|`PLUS(a,b)      -> let a = compile_expr(a, thr) in
                    let b = compile_expr(b, thr) in
		    compile_add (a, b, thr)
|`MINUS(a,b)     -> let a = compile_expr(a, thr) in
                    let b = compile_expr(b, thr) in
		    compile_sub (a, b, thr)
|`TIMES(a,b)     -> let a = compile_expr(a, thr) in
                    let b = compile_expr(b, thr) in
		    compile_mul (a, b, thr)
|`DIV(a,b)       -> let a = compile_expr(a, thr) in
                    let b = compile_expr(b, thr) in
		    compile_div (a, b, thr)
|`DOTPROD(a,b)   -> let a = compile_expr(a, thr) in
                    let b = compile_expr(b, thr) in
		    compile_dot (a, b, thr)
|`CROSSPROD(a,b) -> let a = compile_expr(a, thr) in
                    let b = compile_expr(b, thr) in
		    compile_xpd (a, b, thr)
|`FLOAT(c)       -> (thr#flt_comp(`F_CONST(c)) :> compiled_expr)
|`STRING(c)      -> (thr#str_comp(`S_CONST(c)) :> compiled_expr)
|`UMINUS(e)      -> compile_expr(`TIMES(`FLOAT(-1.0),e), thr)
|`ASG(`SCALAR(v),op,e) -> 
                     let e = compile_expr(e, thr) in
		     let (is_array, ti) = thr#var_typeinfo v in
		     if is_array
		     then
		         failwith (sprintf "Input error: Scalar assignment to %s, which is an array" v)
		     else
		         let vv = compile_var_ld(v, thr) in
		         let e  = compile_asgop(vv, op, e, thr) in
			 compile_var_st (v, e, thr)
|`ID(v)          -> let (is_array, ti) = thr#var_typeinfo v in
                     if is_array
		     then failwith (sprintf "Input error: Array %s used as a scalar" v)
                     else compile_var_ld(v, thr)

|`ASG(`ARRAYELT(v,i),op,e) -> failwith "FIXME: Can't compile array assignment"
|`MUX(_,_,_)       -> failwith "FIXME: ?: not implemented yet"
|`ARR(_,_)         -> failwith "FIXME: array fetch not implemented yet"
|`CAST(tc,e)       -> let e = compile_expr(e, thr) in
                      let typ  = (match tc with
		      | `FLOAT -> `FLOAT
		      | `STRING -> `STRING
		      | `COLOR(None) -> `COLOR
		      | `POINT(None) -> `VEC(`POINT)
		      | `VECTOR(None) -> `VEC(`VECTOR)
		      | `NORMAL(None) -> `VEC(`NORMAL)
		      | `MATRIX(None) -> `MATRIX
		      | `COLOR(Some _)
		      | `POINT(Some _)
		      | `VECTOR(Some _)
		      | `NORMAL(Some _)
		      | `MATRIX(Some _) -> failwith "FIXME: typecasts to explicitly specified coordinate spaced not supported yet"
		      )
		      in
                      let temp = thr#new_any_temp typ in
		      compile_temp_asg (temp, e, thr);
		      (temp :> compiled_expr)
(* FIXME: add some minimal polymorphism support *)
(* ASSUMPTION: List.map iterates from 0th element to the last one in order.
               We want to have well-defined order of evaluation here. *)
|`PROCCALL(n,args) -> let args = List.map (fun arg -> compile_expr (arg, thr)) args in
		      let args = List.map (
		      function
		      | (#any_temp as e) -> e
		      | `TUPLE(_) -> failwith "FIXME: Bare tuples can't be used as function arguments (yet)"
		      ) args in
		      (* FIXME: This is totally broken.
                       * We assume that all functions return floats,
                       * except for the functions listed here that
                       *)
                      if
                      (
                          (* Always return vectors *)
                          (n = "normalize" || n = "trace_instr" || n = "get_hit" || n = "load" || n = "get_Ng" || n = "get_N") ||
                          (* Polymorphic - either all-vec, or all-flt, or all-color
                           * Should support flt->vec and flt->color casts, but don't need to. 
                           *)
                          ((n = "mix" || n = "max" || n = "min" || n = "clamp") &&
                           (List.for_all (function|#flt_temp -> true|#vec_temp -> true|_ -> false) args) &&
                           (List.exists (function|#vec_temp -> true|_ -> false) args)
                          ) ||
                          (* Polymorphic in first argument *)
                          ((n = "update_xcomp" || n = "update_ycomp" || n = "update_zcomp") &&
                           (match (List.hd args) with #col_temp -> false | _ -> true)
                          )
                      )
                      then
                          (thr#vec_comp (`V_FUN(n,args), `POINT) :> compiled_expr)
                      else if 
                      (
                          (n = "get_Cs" || n = "trace") ||
                          (* Polymorphic - either all-vec, or all-flt, or all-color
                           * Should support flt->vec and flt->color casts, but don't need to. 
                           *)
                          ((n = "mix" || n = "max" || n = "min" || n = "clamp") &&
                           (List.for_all (function|#flt_temp -> true|#col_temp -> true|_ -> false) args) &&
                           (List.exists (function|#col_temp -> true|_ -> false) args)
                          )
                          ||
                          (* Polymorphic in first argument *)
                          ((n = "update_xcomp" || n = "update_ycomp" || n = "update_zcomp") &&
                           (match (List.hd args) with #col_temp -> true | _ -> false)
                          )
                      )
                      then
                          (thr#col_comp (`C_FUN(n,args)) :> compiled_expr)
                      else
                      (
                          try
                          (* TODO: USE full ABI information, not only return type *)
                          let (rt,_) = ht_get abi_registry n in
                              match rt with
                              |`FLOAT   -> (thr#flt_comp (`F_FUN(n,args)) :> compiled_expr)
                              |`COLOR   -> (thr#col_comp (`C_FUN(n,args)) :> compiled_expr)
                              |`STRING  -> (thr#str_comp (`S_FUN(n,args)) :> compiled_expr)
                              |`MATRIX  -> (thr#mtx_comp (`M_FUN(n,args)) :> compiled_expr)
                              |`VEC(vt) -> (thr#vec_comp (`V_FUN(n,args),vt) :> compiled_expr)
                              |`VOID    -> failwith "FIXME: void functions not supported yet"
                          with Not_found ->
                              (thr#flt_comp (`F_FUN(n,args)) :> compiled_expr)
                      )


|`TEXTURE(_,_,_,_) -> failwith "FIXME: texture access not implemented yet"
(* ASSUMPTION: List.map iterates from 0th element to the last one in order.
               We want to have well-defined order of evaluation here. *)
|`TUPLE(vs)        -> let vsc = List.map (fun arg -> compile_expr (arg, thr)) vs in
		      let vsc = List.map (
		      function
		      | (#flt_temp as e) -> e
		      | e -> type_error_1 "Only float expressions can be used as tuple elements, not %s" e
		      ) vsc in
		      (`TUPLE(vsc))

(* thr is DEAD after running this function and needs to be revived *)
and compile_relation : (Ast.relation * thread * thread * thread -> unit)
= fun (e, thr, then_thr, else_thr) -> match e with
| `GT(a,b) -> let a = compile_expr(a, thr) in
              let b = compile_expr(b, thr) in
	      (match(a,b) with
	      |(#flt_temp as a),(#flt_temp as b) ->
	              thr#conditional_join_and_die(`GT(a,b), then_thr, else_thr)
	      |(a,b) -> type_error "Only floats can be compared by >/>=, tried to do %s > %s" a b
	      )
| `GE(a,b) -> let a = compile_expr(a, thr) in
              let b = compile_expr(b, thr) in
	      (match(a,b) with
	      |(#flt_temp as a),(#flt_temp as b) ->
	              thr#conditional_join_and_die(`GE(a,b), then_thr, else_thr)
	      | (a,b) -> type_error "Only floats can be compared by >/>=, tried to do %s >= %s" a b
	      )
(* At this point a and b have no side effects, so a>b and b<a are equivalent 
 * We cannot do this translation earlier, as we want foo()>bar() to run foo() first
 * and bar()<foo() to run bar() first, for greater debugability
 *)
| `LT(a,b) -> let a = compile_expr(a, thr) in
              let b = compile_expr(b, thr) in
	      (match(a,b) with
	      |(#flt_temp as a),(#flt_temp as b) ->
	              thr#conditional_join_and_die(`GT(b,a), then_thr, else_thr)
	      |(a,b) -> type_error "Only floats can be compared by >/>=, tried to do %s < %s" a b
	      )
| `LE(a,b) -> let a = compile_expr(a, thr) in
              let b = compile_expr(b, thr) in
	      (match(a,b) with
	      |(#flt_temp as a),(#flt_temp as b) ->
	              thr#conditional_join_and_die(`GE(b,a), then_thr, else_thr)
	      | (a,b) -> type_error "Only floats can be compared by >/>=, tried to do %s <= %s" a b
	      )
| `EQEQ(a,b) ->
              let a = compile_expr(a, thr) in
              let b = compile_expr(b, thr) in
	      (match(a,b) with
	      |(#flt_temp as a),(#flt_temp as b) ->
	              thr#conditional_join_and_die(`FEQ(a,b), then_thr, else_thr)
	      |(#vec_temp as a),(#vec_temp as b) ->
	              thr#conditional_join_and_die(`VEQ(a,b), then_thr, else_thr)
	      |(#col_temp as a),(#col_temp as b) ->
	              thr#conditional_join_and_die(`CEQ(a,b), then_thr, else_thr)
	      |(#mtx_temp as a),(#mtx_temp as b) ->
	              thr#conditional_join_and_die(`MEQ(a,b), then_thr, else_thr)
	      |(#str_temp as a),(#str_temp as b) ->
	              thr#conditional_join_and_die(`SEQ(a,b), then_thr, else_thr)
	      | _ -> type_error "FIXME: %s and %s compared by ==/!=, can compile only values of the same type (yet?)" a b
	      (* FIXME: cast floats to the right types, and handle tuples *)
	      )
| `NE(a,b)  -> compile_relation((`EQEQ(a,b)), thr, else_thr, then_thr)
| `NEG(r)   -> compile_relation(r, thr, else_thr, then_thr)
| `AND(a,b) -> let mid_thr = thr#fork_thread in
	       let _ = compile_relation(a, thr, mid_thr, else_thr) in
	       let _ = compile_relation(b, mid_thr, then_thr, else_thr) in
	       ()
| `OR(a,b)  -> let mid_thr = thr#fork_thread in
	       let _ = compile_relation(a, thr, then_thr, mid_thr) in
	       let _ = compile_relation(b, mid_thr, then_thr, else_thr) in
	       ()

let loop_ctl : (thread * thread) list ref = ref []
let loop_continue_point level = fst (List.nth (!loop_ctl) (level-1))
let loop_break_point level    = snd (List.nth (!loop_ctl) (level-1))
let start_loop (continue_thr, break_thr) = loop_ctl := (continue_thr, break_thr)::!loop_ctl
let end_loop () = loop_ctl := List.tl !loop_ctl

let rec compile_statement : (Ast.statement * thread) -> unit
= fun (s, thr) -> match s with
(*
 * RETURN(e) handling is somewhat tricky. What we actually do is:
 * 1. evaluate e
 * 2. assign the results to the "return variable" (like in Pascal)
 * 3. jump to the end
 * 4. make a bogus label, so that any instructions that follow will be placed on it
 *    (they are all obviously dead code)
 *)
| `RETURN(e) -> let e = compile_expr (e, thr) in
                (match thr#return with
		| (Some(temp)) ->
		    compile_temp_asg(temp, e, thr);
		    thr#register_return;
		| (None)       -> failwith "Input error: return with a value used in a function not returning anything"
                )
| `BREAK(level)    -> thr#join_and_dev_null(loop_break_point level)
| `CONTINUE(level) -> thr#join_and_dev_null(loop_continue_point level)
| `IFELSE(r,a,b) ->
                let then_thr = thr#fork_thread in
		let else_thr = thr#fork_thread in
		compile_relation (r, thr, then_thr, else_thr);
		compile_statement (`BLOCK(a), then_thr);
		compile_statement (`BLOCK(b), else_thr);
		if then_thr#dev_null && else_thr#dev_null
		then
		(
		    thr#restart;
		    thr#go_to_dev_null
		)
		else
		(
		    thr#restart;
		    then_thr#join_and_die thr;
		    else_thr#join_and_die thr;
		)
| `WHILE(r,s) ->
		let loop_body_thr = thr#fork_thread in
		let exit_thr      = thr#fork_thread in
		let loop_thr      = thr#fork_thread in
		thr#join_and_die loop_thr;

		start_loop (thr, exit_thr);
		compile_relation (r, loop_thr, loop_body_thr, exit_thr);
		loop_body_thr#join_and_die thr;
		end_loop();

		thr#restart;
		exit_thr#join_and_die thr;
| `FOR(init,cond,next,body) ->
		(* This is pretty silly, syntactically it is an expression, *
		 * but semantically a statement. Well, C started it ...     *)
		ignore (compile_expr (init, thr));

		let loop_body_thr     = thr#fork_thread in
		let loop_cont_thr     = thr#fork_thread in
		let exit_thr          = thr#fork_thread in
		let loop_thr          = thr#fork_thread in
		thr#join_and_die loop_thr;

		start_loop (loop_cont_thr, exit_thr);
		compile_relation (cond, loop_thr, loop_body_thr, exit_thr);
		compile_statement(`BLOCK(body), loop_body_thr);
		loop_body_thr#join_and_die loop_cont_thr;
		ignore (compile_expr (next, loop_cont_thr)); (* The same silliness again *)
		loop_cont_thr#join_and_die thr;
		end_loop();

		thr#restart;
		exit_thr#join_and_die thr;
| `BLOCK([]) -> ()
| `BLOCK(hd::tl) -> compile_statement (hd, thr);
                    compile_statement (`BLOCK(tl), thr)

(* ASSUMPTION: We assume that List.map executes f in order *)
| `PROCCALL(nam,vs) -> let vsc =
                         List.map
			     (fun v ->
			          let vc = compile_expr(v, thr) in
				  match vc with
				  | (#any_temp as vc) -> vc
				  | `TUPLE(_) -> failwith "FIXME: Bare tuples not allowed as function arguments (yet)"
		             ) vs in
		     thr#register_statement (`PROCCALL(nam, vsc))
| `ASG(a,b,c) -> ignore(compile_expr(`ASG(a,b,c), thr))
| `VARDEF(ext,typ,nam,None) ->
			       thr#add_local_variable nam (ext, typ)
| `VARDEF(ext,typ,nam,Some e) ->
			       thr#add_local_variable nam (ext, typ);
			       let e = compile_expr(e, thr) in
			       ignore(compile_var_st(nam, e, thr))
| `SOLAR(a,b)       -> failwith "FIXME: Can't compile solar (yet)"
| `ILLUMINATE(a,b)  -> failwith "FIXME: Can't compile illuminate (yet)"
| `ILLUMINANCE(a,b) -> failwith "FIXME: Can't compile illuminance (yet)"
