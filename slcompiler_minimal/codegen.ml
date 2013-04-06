(********************************************************************)
(* This module receives (hopefully) well-optimized code in internal *)
(* representation (SSA three-address code) and produces RPU asm     *)
(* output.                                                          *)
(*                                                                  *)
(* It can (optimistically) assume that:                             *)
(* * Code is in SSA form                                            *)
(* * No dead/unreachable code is present                            *)
(* * Everything is as optimized as possible                         *)
(*                                                                  *)
(* So the things codegen has to do are:                             *)
(* * Generate instructions, using virtual registers                 *)
(* * Group instructions into basic blocks, to get rid of some jumps *)
(* * Classify virtual registers as constant, single-use, etc.       *)
(* * Run a peephole optimizer to reduce register pressure           *)
(* * Extract information for register allocation                    *)
(* * Allocate registers                                             *)
(* * Remove extra MOVs                                              *)
(* * (Schedule instructions)                                        *)
(* * Do low-level optimizations using a peephole optimizer          *)
(*                                                                  *)
(********************************************************************)

open Ast2
open Printf
open Util

type ('a,'b) allocator = (('a,'b) ht * (int ref) * (int->'a->'b))

let allocator_new : (int->'a->'b) -> ('a,'b) allocator
= fun f ->
    ((ht_new ()), (ref 0), f)

let allocator_alloc : ('a,'b) allocator -> 'a -> 'b
= fun (ht,r,f) k ->
    ht_get_orelse ht k
    (fun () ->
		let r_v = !r in
		r:=r_v+1;
		let frk = (f r_v k) in
		ht_set ht k frk;
        frk
    )

type bb = {
	bb_instr        : string list da;
	mutable bb_next : string option;
	mutable bb_then : string option;
}
let bb_new () = { bb_instr = da_new (); bb_next = None; bb_then = None }

type bbs = (string, bb) ht
let bbs_new : unit -> bbs = fun () -> ht_new ()

(*
 * Well, we want to have *some* code fast, so let's assume the API in
 * R0.xyz - P
 * R1.xyz - Eye
 * R2.xyz - near_color
 * R3.xyz - far_color
 *
 * R0.xyz - result
 *)
let codegen_function_x
= fun (rt, name, _, (params, _), (body, _, start_cpt, end_cpt), (epilogue_phi_in, retval, _)) ->
let param_to_string i = sprintf "R%d.xyz" i in
let reg_allocator : ('a, string) allocator = allocator_new
    (fun k temp -> match temp with
    |`FLT_TEMP(_) -> sprintf "VR%d.w" k
	|`VEC_TEMP(_) -> sprintf "VR%d.xyz" k
	|`COL_TEMP(_) -> sprintf "VR%d.xyz" k
	| _ -> failwith "FIXME: Only float/vector/color operations supported"
	) in
let alloc_reg temp = allocator_alloc reg_allocator temp in
(* FIXME: The phi-transfer should be simultaneous
 * while(...) {
 *   c = a;
 *   a = b;
 *   b = c;
 * }
 *
 * Gets compiled to:
 * {
 *   a2 = phi(a0, a1);
 *   b2 = phi(b0, b1);
 *   c2 = phi(c0, c1);
 *
 *   c1 = a2;
 *   a1 = b2;
 *   b1 = c1;
 * }
 *
 * And simplified to:
 * {
 *   a2 = phi(a0, b2);
 *   b2 = phi(b0, a2);
 * }
 *
 * So this code is buggy: 
 *)
let vr (temp:[<any_temp]) = (alloc_reg (temp :> any_temp)) in

let cur_bb = ref(bb_new ()) in
let bbs = bbs_new () in
ht_set_unique bbs "Lin" (!cur_bb);
let bb_start lbl =
(
	cur_bb := bb_new ();
	ht_set_unique bbs lbl (!cur_bb)
) in
let out s = da_push (!cur_bb).bb_instr s in
let bb_end lbl = (!cur_bb).bb_next <- Some lbl in

let out_link_out (jmp_target, phi_out) =
    (
	let phi_in = if jmp_target = end_cpt then epilogue_phi_in
		    else (let (phi_in, _, _) = ht_get body jmp_target in phi_in)
	in
	List.iter2 (fun dst src ->
	    out ["mov"; (alloc_reg dst); (alloc_reg src)]
	) phi_in phi_out;
	if jmp_target = end_cpt
	then
	(
	    (match retval with
	    | None -> ()
	    | Some retval -> out ["mov"; "R0"; (alloc_reg retval)]
	    );
	    bb_end "return"
	)
	else
	    bb_end (sprintf "L%d" jmp_target)
    )
in
let mov d s   = out ["mov"; d; s] in
let add d a b = out ["add"; d; a; b] in
let sub d a b = out ["sub"; d; a; b] in
let mul d a b = out ["mul"; d; a; b] in
let dp3 d a b = out ["dp3"; d; a; b] in
let out_link_dual t e = (
	(!cur_bb).bb_then <- Some t;
	(!cur_bb).bb_next <- Some e;
)
in
(
    printf "; function %s\n" name;
    (*bb_start "Lin";*)
    list_iteri (fun i temp ->
	let reg = alloc_reg temp in
	out ["mov"; reg; (param_to_string i)]
    ) params;
    bb_end (sprintf "L%d" start_cpt);

    ht_iter (fun k (link_in, op, link_out) ->
    bb_start (sprintf "L%d" k);

	(* link_in is actually not used here, but by *)
	(match op with
	|`FCOMP(dst, `F_ASG(src)) -> mov (vr dst) (vr src)
	|`FCOMP(dst, `F_CONST(f)) -> mov (vr dst) (sprintf "%f" f)
	|`FCOMP(dst, `F_DOT_VV(a, b)) -> dp3 (vr dst) (vr a) (vr b)

	|`VCOMP(dst, `V_FLTCAST(src)) -> mov (vr dst) (vr src)
	|`VCOMP(dst, `V_ADD(a, b)) -> add (vr dst) (vr a) (vr b)
	|`VCOMP(dst, `V_SUB(a, b)) -> sub (vr dst) (vr a) (vr b)
	|`VCOMP(dst, `V_MUL(a, b)) -> mul (vr dst) (vr a) (vr b)

	|`CCOMP(dst, `C_FLTCAST(src)) -> mov (vr dst) (vr src)
	|`CCOMP(dst, `C_ADD(a, b)) -> add (vr dst) (vr a) (vr b)
	|`CCOMP(dst, `C_SUB(a, b)) -> sub (vr dst) (vr a) (vr b) 
	|`CCOMP(dst, `C_MUL(a, b)) -> mul (vr dst) (vr a) (vr b)
	
	|`FCOMP(dst, `F_FUN("sqrt", [src])) ->
		out ["mov_rsq"; "R15.w"; vr src];
		out ["mov_rcp"; "R15.w"; "S.w"];
		out ["mov"; vr dst; "S.w"]
	|`CJMP(`GT(a,b), link_out_then) ->
		out ["+"; "sub"; "R15.w"; (vr a); (vr b); "+"; "jmp"; "w"; "(>0)"];
		out_link_dual (sprintf "LT%d" k) (sprintf "LE%d" k);
	|`CJMP(`GE(a,b), link_out_then) ->
		out ["+"; "sub"; "R15.w"; (vr a); (vr b); "+"; "jmp"; "w"; "(>=0)"];
		out_link_dual (sprintf "LT%d" k) (sprintf "LE%d" k);
	|`CJMP(`FEQ(a,b), link_out_then) ->
		out ["+"; "sub"; "R15.w"; (vr a); (vr b); "+"; "jmp"; "w"; "(=0)"];
		out_link_dual (sprintf "LT%d" k) (sprintf "LE%d" k);
	|`CJMP(`VEQ(a,b), link_out_then) ->
		out ["+"; "sub"; "R15.w"; (vr a); (vr b); "+"; "jmp"; "w"; "(=0)"];
		out_link_dual (sprintf "LT%d" k) (sprintf "LE%d" k);
	|`CJMP(`CEQ(a,b), link_out_then) ->
		out ["+"; "sub"; "R15.w"; (vr a); (vr b); "+"; "jmp"; "w"; "(=0)"];
		out_link_dual (sprintf "LT%d" k) (sprintf "LE%d" k);
	|`CJMP(_, link_out_then) ->
		out ["+"; "?"; "jmp"; "xyzw"; "(>0)"];
		out_link_dual (sprintf "LT%d" k) (sprintf "LE%d" k);
	|`NOP -> ()
	| _   -> out ["???"]
	)
	;
	(match op with
	|`CJMP(_, link_out_then) -> (
		bb_start (sprintf "LE%d" k);
		out_link_out link_out;
		bb_start (sprintf "LT%d" k);
		out_link_out link_out_then
		)
	|_->out_link_out link_out;
	)
    ) body;
    (* Now bbs contains basic blocks, but there are too many
     * We should do a consolidation run.
     *
     * A label starts a basic block if any of:
     * * it is a start label
     * * more than one jump points to it
     * * a jump-then or jump-else points to it
     *
     * TODO: all empty block should also be removed
     *       For now they aren't, for example:
     *       A: blah; jmp E
     *       B: blah; add r0,r1,r2 + jmp D, w, (>0)
     *       D; jmp E
     *
     *       D is not removed !
     *)
    let bb_start = set_new () in
    set_set bb_start "Lin";
    let ptr_counter = ht_new () in
	ht_iter (fun _ bb ->
		let n = (match bb.bb_next with Some n -> n | None -> failwith "Internal error: basic block without next") in
		match bb.bb_then with
		| Some t ->
			set_set bb_start n;
			set_set bb_start t
		| None ->
			ht_set ptr_counter n (1 + (ht_get_with_default ptr_counter n 0))
	) bbs;
	ht_iter (fun bb_label cnt ->
		if cnt > 1
		then set_set bb_start bb_label
	) ptr_counter;
    (* Now we know which bbs should stay and which should be integrated *)
    set_iter (fun bb_label ->
      let rec iter () = 
      (
    	let bb = ht_get bbs bb_label in
    	let n  = (match bb.bb_next with Some n -> n | None -> failwith "Internal error: basic block without next") in
		if (n = "return" || set_mem bb_start n)
		then () (* nothing to do *)
		else
		(
			let bb_n = ht_get bbs n in
			bb.bb_next <- bb_n.bb_next;
			(if bb.bb_then = None
			then ()
			else failwith "Internal error: Trying to integrate a basic block that ends with a jump");
			bb.bb_next <- bb_n.bb_next;
			bb.bb_then <- bb_n.bb_then;
			da_append_da bb.bb_instr bb_n.bb_instr;
			ht_del bbs n;
			iter ()
		)
	  )
      in
      iter ()
    ) bb_start;
	(*set_iter (fun k -> printf "SBB: %s\n" k) bb_start;*)

(* ... *)

    (* now we have instructions in output *)
    ht_iter (fun k bb ->
    	printf "%s:\n" k;
    	da_iter (fun instr -> printf "  %s\n" (list_join " " instr)) bb.bb_instr;
        printf "  jmp %s%s\n" (match bb.bb_next with Some s -> s | None -> "?")
                              (match bb.bb_then with Some s -> sprintf " | %s" s | None -> "")
    ) bbs
)

let codegen : Ast2.definition list -> string
= fun defs ->
    list_join "\n" (List.map (function
	|`SHADER(st, name, params, prelude, body, epilogue)   -> sprintf "Shader %s\n" name
	|`FUNCTION(rt, name, params, prelude, body, epilogue) ->
	(
		codegen_function_x (rt, name, params, prelude, body, epilogue);
		""
	)
    ) defs)
