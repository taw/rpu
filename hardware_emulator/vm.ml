open Opcodes
open Printf

(* int_of_float round towards 0, what's pretty braindead *)
let int_of_float_floor x = int_of_float (floor x)

(* FIXME: OCaml doesn't seem to have nearest-even IEEE rounding mode !!!
 * This one "almost" works
 *)
let round_ne x = floor (x+.0.5)

(* Size of the register file *)
let reg_count      = 56
(* Size of the visible stack window *)
let stack_window   = 8

type reg = { mutable x : float;
             mutable y : float;
	     mutable z : float;
	     mutable w : float}
(* To use reg as an array
 * argument must be 0..3
 * TODO: if it's too slow, convert to array access
 *)
let reg_geti reg = function
| 0 -> reg.x
| 1 -> reg.y
| 2 -> reg.z
| 3 -> reg.w
| _ -> failwith "Internal error: reg_geti argument out of range"

let reg_to_v3 r = Math3.v3_make r.x r.y r.z

let memory_fetch_reg = Memory.fetch_cell4 (fun x y z w -> {x=x;y=y;z=z;w=w})
let memory_store_reg mem addr v = Memory.store_cell4 mem addr v.x v.y v.z v.w

type recctl = int * int (* return address, how much to pop *)

type stack = {
    stack_data : reg DynArray.t; (* X X X S7 .. S0 *)
    stack_ctrl : recctl DynArray.t;
}

let stack_new () = {
    stack_data = DynArray.init stack_window (fun _ -> { x = 0.0; y = 0.0; z = 0.0; w = 1.0 });
    stack_ctrl = DynArray.create ();
}

let stack_incr stack n =
    for i = 0 to n-1 do
	DynArray.add stack.stack_data { x = 0.0; y = 0.0; z = 0.0; w = 1.0 }
    done

let stack_decr stack n =
    for i = 0 to n-1 do
	DynArray.delete_last stack.stack_data
    done

let stack_end stack =
    let (ret_addr, pop_cnt) = DynArray.last stack.stack_ctrl in
    (
	DynArray.delete_last stack.stack_ctrl;
	stack_decr stack pop_cnt;
	ret_addr;
    )

let stack_begin stack ret_addr pop_cnt =
(
    DynArray.add stack.stack_ctrl (ret_addr, pop_cnt);
    stack_incr stack pop_cnt;
)    

let stack_return_ends_shader stack =
    DynArray.empty stack.stack_ctrl

(* i \in 0..7 *)
let stack_getreg stack i =
    DynArray.get stack.stack_data (DynArray.length stack.stack_data + i - 8)

(* i \in 0..7 *)
let stack_setreg stack i v =
    DynArray.set stack.stack_data (DynArray.length stack.stack_data + i - 8) v

type texture_data = [
| `FLT3 of float array
| `FLT4 of float array
| `BYTE3 of char array
]

(*
 * If texture goes from 0 to 1, then
 *               tx(0-e), tx(1+e), tx(2-e), tx(2+e) equal
 * CLAMP:        tx(0)    tx(1)    tx(1)    tx(1)
 *      /---
 *     /                
 * ---/
 * PERIODIC:     tx(1-e)  tx(e)    tx(1-e)  tx(e)
 *   /  /  / 
 *  /  /  /              
 * /  /  /
 * INV_PERIODIC: tx(e)    tx(1-e)  tx(e)    tx(e)
 * \    /\ 
 *  \  /  \ 
 *   \/    \ 
 *)
type texture_mode = [
| `CLAMP
| `PERIODIC
| `INV_PERIODIC
]
type interpolation_mode = [
| `NEAREST
| `LINEAR
]
type texture = {
    tx_data : texture_data;
    tx_xsz  : int;
    tx_ysz  : int;
    tx_modx : texture_mode;
    tx_mody : texture_mode;
    tx_intx : interpolation_mode;
    tx_inty : interpolation_mode;
    tx_xmul : float;
    tx_ymul : float;
}

let texdata_load : texture_data -> int -> int -> int -> reg
= fun tx x y xsz -> match tx with
| `FLT3(data)  -> let addr = (y * xsz + x) * 3 in
                  {x=data.(addr); y=data.(addr+1); z=data.(addr+2); w=1.0}
| `FLT4(data)  -> let addr = (y * xsz + x) * 4 in
                  {x=data.(addr); y=data.(addr+1); z=data.(addr+2); w=data.(addr+2)}
| `BYTE3(data) -> let addr = (y * xsz + x) * 3 in
		  let translation_factor = 1.0 /. 255.0 in
                  let float_of_byte x = translation_factor *. float_of_int (Char.code x) in
                  {x=float_of_byte data.(addr);
		   y=float_of_byte data.(addr+1);
		   z=float_of_byte data.(addr+2);
		   w=1.0}

let tx_transform_nn : float -> float -> texture_mode -> int -> int
= fun a amul amod asz ->
    let a = a*.amul +. 0.5 in
    let a = int_of_float_floor a in
    match amod with
    | `CLAMP        -> (if a < 0 then 0 else if a > asz-1 then asz-1 else a)
    | `PERIODIC     -> (a mod asz)
    | `INV_PERIODIC -> let a_dual_period = 2*asz-2 in
		       (let ap2 = a mod a_dual_period in
		        if ap2 >= asz then a_dual_period - ap2 else ap2)

let tx_transform2 : float -> float -> texture_mode -> int -> (int*int)
= fun a amul amod asz ->
    let a  = a*.amul in
    let ad = int_of_float_floor a in
    let au = ad + 1 in
    match amod with
    | `CLAMP        -> (
			(if ad < 0 then 0 else if ad > asz-1 then asz-1 else ad),
		        (if au < 0 then 0 else if au > asz-1 then asz-1 else au)
		       )
    | `PERIODIC     -> (ad mod asz, au mod asz)
    | `INV_PERIODIC -> let a_dual_period = 2*asz-2 in
		       (
		       (let adp2 = ad mod a_dual_period in
		        if adp2 >= asz then a_dual_period - adp2 else adp2),
		       (let aup2 = au mod a_dual_period in
		            if aup2 >= asz then a_dual_period - aup2 else aup2)
		       )

let tx_transform3 : float -> float -> texture_mode -> int -> (int*int*float)
= fun a amul amod asz ->
    let a  = a*.amul in
    let af = a -. floor a in
    let a  = floor a in
    let ad = int_of_float a in
    let au = ad + 1 in
    match amod with
    | `CLAMP        -> (
			(if ad < 0 then 0 else if ad > asz-1 then asz-1 else ad),
		        (if au < 0 then 0 else if au > asz-1 then asz-1 else au),
			af
		       )
    | `PERIODIC     -> (ad mod asz, au mod asz, af)
    | `INV_PERIODIC -> let a_dual_period = 2*asz-2 in
		       (
		       (let adp2 = ad mod a_dual_period in
		        if adp2 >= asz then a_dual_period - adp2 else adp2),
		       (let aup2 = au mod a_dual_period in
		            if aup2 >= asz then a_dual_period - aup2 else aup2),
			af
		       )

let texture_load4 : texture -> float -> float -> (reg * reg * reg * reg)
= fun tx x y ->
    let (xd,xu) = tx_transform2 x tx.tx_xmul tx.tx_modx tx.tx_xsz in
    let (yd,yu) = tx_transform2 y tx.tx_ymul tx.tx_mody tx.tx_ysz in
    (
	texdata_load tx.tx_data xd yd tx.tx_xsz,
	texdata_load tx.tx_data xu yd tx.tx_xsz,
	texdata_load tx.tx_data xd yu tx.tx_xsz,
	texdata_load tx.tx_data xu yu tx.tx_xsz
    )

let regmix f a b = let inv_f = 1.0 -. f in
{
    x = inv_f *. a.x +. f *. b.x;
    y = inv_f *. a.y +. f *. b.y;
    z = inv_f *. a.z +. f *. b.z;
    w = inv_f *. a.w +. f *. b.w;
}

let texture_load : texture -> float -> float -> reg
= fun tx x y ->
    match (tx.tx_intx, tx.tx_inty) with
    |(`NEAREST, `NEAREST) ->
	let x = tx_transform_nn x tx.tx_xmul tx.tx_modx tx.tx_xsz in
	let y = tx_transform_nn y tx.tx_ymul tx.tx_mody tx.tx_ysz in
	texdata_load tx.tx_data x y tx.tx_xsz
    |(`LINEAR, `NEAREST)  ->
	let (xd,xu,xf) = tx_transform3 x tx.tx_xmul tx.tx_modx tx.tx_xsz in
	let y = tx_transform_nn y tx.tx_ymul tx.tx_mody tx.tx_ysz in
	    (regmix xf (texdata_load tx.tx_data xd y tx.tx_xsz)
	    	       (texdata_load tx.tx_data xu y tx.tx_xsz))
    |(`NEAREST, `LINEAR)  ->
	let x = tx_transform_nn x tx.tx_xmul tx.tx_modx tx.tx_xsz in
        let (yd,yu,yf) = tx_transform3 y tx.tx_ymul tx.tx_mody tx.tx_ysz in
	    (regmix yf (texdata_load tx.tx_data x yd tx.tx_xsz)
	    	       (texdata_load tx.tx_data x yu tx.tx_xsz))
    |(`LINEAR, `LINEAR)   -> 
	let (xd,xu,xf) = tx_transform3 x tx.tx_xmul tx.tx_modx tx.tx_xsz in
        let (yd,yu,yf) = tx_transform3 y tx.tx_ymul tx.tx_mody tx.tx_ysz in
	regmix yf 
	    (regmix xf (texdata_load tx.tx_data xd yd tx.tx_xsz)
	    	       (texdata_load tx.tx_data xu yd tx.tx_xsz))
	    (regmix xf (texdata_load tx.tx_data xd yu tx.tx_xsz)
	               (texdata_load tx.tx_data xu yu tx.tx_xsz))

type vm =
{
    code_mem   : op DynArray.t;
    data_mem   : Memory.memory;
    regs       : reg array;
    (* Special registers
     * Fixme: it would be inconvenient to keep them in regs,
     *        but maybe merge them with A to form a 6-component addressing register ?
     *)
    mutable triaddr : float;
    mutable objaddr : float;
    stack      : stack;
    textures   : texture DynArray.t;
    mutable pc : int;
}

let vm_new () = {
    code_mem   = DynArray.create ();
    data_mem   = DynArray.create ();
    regs       = Array.init reg_count (fun _ -> { x = 0.0; y = 0.0; z = 0.0; w = 1.0 });
    triaddr    = 0.0;
    objaddr    = 0.0;
    stack      = stack_new ();
    textures   = DynArray.create ();
    pc         = -1;
}

let vm_setreg vm reg_index x y z w =
    vm.regs.(reg_index) <- {x=x; y=y; z=z; w=w}

let vm_setsubreg vm reg sub v =
    match reg with
    | `Reg(reg_index) ->
        let r = vm.regs.(reg_index) in
        let x,y,z,w = (r.x, r.y, r.z, r.w) in
        (match sub with
        | 0 -> vm.regs.(reg_index) <- {x=v; y=y; z=z; w=w}
        | 1 -> vm.regs.(reg_index) <- {x=x; y=v; z=z; w=w}
        | 2 -> vm.regs.(reg_index) <- {x=x; y=y; z=v; w=w}
        | 3 -> vm.regs.(reg_index) <- {x=x; y=y; z=z; w=v}
        | _ -> failwith (sprintf "Internal error: something other than 0..3 passed as register field number to vm_setsubreg: %d" sub)
        )
    | `Stack(_) ->
        failwith "FIXME: Stack setting not implemented yet"

let vm_setst vm i x y z w =
    stack_setreg vm.stack i {x=x; y=y; z=z; w=w}

let vm_texload vm txd coords =
    texture_load (DynArray.get vm.textures (int_of_float txd.x)) coords.x coords.y

let vm_texload4 vm txd coords =
    texture_load4 (DynArray.get vm.textures (int_of_float txd.x)) coords.x coords.y

let addr_geti vm a =
    if a < 4
    then int_of_float (reg_geti vm.regs.(reg_index_A) a)
    else if a = 4
    then int_of_float vm.triaddr
    else if a = 5
    then int_of_float vm.objaddr
    else failwith "Internal error: A index out of 0..5 range (Ax, Ay, Az, Aw, triaddr, objaddr)"

let vm_fetch =
fun vm -> function
| `Imm f -> { x = f; y = f; z = f; w = f }
| `Reg(sreg,sw,sm) ->
    let rval = (match sreg with
    | `Reg i   -> vm.regs.(i)
    | `Stack i -> stack_getreg vm.stack i
    | `HIT_TRI -> let v = vm.triaddr in {x=v; y=v; z=v; w=v}
    | `HIT_OBJ -> let v = vm.objaddr in {x=v; y=v; z=v; w=v}
    ) in
    let rval = [|rval.x; rval.y; rval.z; rval.w|] in
    { x = sm *. Array.unsafe_get rval sw.sw_x;
      y = sm *. Array.unsafe_get rval sw.sw_y;
      z = sm *. Array.unsafe_get rval sw.sw_z;
      w = sm *. Array.unsafe_get rval sw.sw_w;
    }

let clamp01 x =
    if x < 0.0
    then 0.0
    else if x > 1.0
    then 1.0
    else x

let vm_writeback_spec vm wbm spec_val =
(
    if wbm.wb_x then vm.regs.(reg_index_S).x <- spec_val.x;
    if wbm.wb_y then vm.regs.(reg_index_S).y <- spec_val.y;
    if wbm.wb_z then vm.regs.(reg_index_S).z <- spec_val.z;
    if wbm.wb_w then vm.regs.(reg_index_S).w <- spec_val.w;
)

let vm_writeback vm (wbr,wbm,spec) wb_val =
(
    let wb_val = (match spec with
    | `NOP -> wb_val
    | `SAT -> {
		x = clamp01 wb_val.x;
		y = clamp01 wb_val.y;
		z = clamp01 wb_val.z;
		w = clamp01 wb_val.w;
              }
    | `RCP(i) -> (let v = 1.0/.(reg_geti wb_val i) in vm_writeback_spec vm wbm {x=v;y=v;z=v;w=v}; wb_val)
    | `RSQ(i) -> (let v = 1.0/.sqrt(reg_geti wb_val i) in vm_writeback_spec vm wbm {x=v;y=v;z=v;w=v}; wb_val)
    ) in
    match wbr with
    | `Reg(i)   ->
    (
	if wbm.wb_x then vm.regs.(i).x <- wb_val.x;
	if wbm.wb_y then vm.regs.(i).y <- wb_val.y;
	if wbm.wb_z then vm.regs.(i).z <- wb_val.z;
	if wbm.wb_w then vm.regs.(i).w <- wb_val.w;
    )
    | `Stack(i) ->
    (
	let r = stack_getreg vm.stack i in
	if wbm.wb_x then r.x <- wb_val.x;
	if wbm.wb_y then r.y <- wb_val.y;
	if wbm.wb_z then r.z <- wb_val.z;
	if wbm.wb_w then r.w <- wb_val.w;
    )
)

let frac v = v -. (floor v)

let op_frac a   = { x = frac a.x; y = frac a.y; z = frac a.z; w = frac a.w}
let op_add  a b = { x = a.x +. b.x; y = a.y +. b.y; z = a.z +. b.z; w = a.w +. b.w; }
let op_mul  a b = { x = a.x *. b.x; y = a.y *. b.y; z = a.z *. b.z; w = a.w *. b.w; }
let op_dp3  a b = let v = a.x *. b.x +. a.y *. b.y +. a.z *. b.z in {x=v; y=v; z=v; w=v;}
let op_dp4  a b = let v = a.x *. b.x +. a.y *. b.y +. a.z *. b.z +. a.w *. b.w in {x=v; y=v; z=v; w=v;}
let op_dp2h a b = let v = a.x *. b.x +. a.y *. b.y +. a.z in {x=v; y=v; z=v; w=v;}
let op_dp3h a b = let v = a.x *. b.x +. a.y *. b.y +. a.z *. b.z +. a.w in {x=v; y=v; z=v; w=v;}
let op_mad  a b c = { x = a.x *. b.x +. c.x;
                      y = a.y *. b.y +. c.y;
		      z = a.z *. b.z +. c.z;
		      w = a.w *. b.w +. c.w; }

let op_arith_compute : vm -> op_arith -> (writeback * reg)
= function vm -> function
|`MOV(wb,s)        -> (wb, (vm_fetch vm s))
|`FRAC(wb,s)       -> (wb, (op_frac (vm_fetch vm s)))
|`ADD(wb,s0,s1)    -> (wb, (op_add  (vm_fetch vm s0) (vm_fetch vm s1)))
|`MUL(wb,s0,s1)    -> (wb, (op_mul  (vm_fetch vm s0) (vm_fetch vm s1)))
|`DP3(wb,s0,s1)    -> (wb, (op_dp3  (vm_fetch vm s0) (vm_fetch vm s1)))
|`DP4(wb,s0,s1)    -> (wb, (op_dp4  (vm_fetch vm s0) (vm_fetch vm s1)))
|`DP2H(wb,s0,s1)   -> (wb, (op_dp2h (vm_fetch vm s0) (vm_fetch vm s1)))
|`DP3H(wb,s0,s1)   -> (wb, (op_dp3h (vm_fetch vm s0) (vm_fetch vm s1)))
|`MAD(wb,s0,s1,s2) -> (wb, (op_mad  (vm_fetch vm s0) (vm_fetch vm s1) (vm_fetch vm s2)))

let cond_matches_scalar cond tested_value =
    if tested_value < 0.0
    then cond.c_lt0
    else if tested_value = 0.0
    then cond.c_eq0
    else if tested_value >= 1.0
    then cond.c_ge1
    else cond.c_b01

let cond_matches cond tested_value =
    let cm_x = cond_matches_scalar cond tested_value.x in
    let cm_y = cond_matches_scalar cond tested_value.y in
    let cm_z = cond_matches_scalar cond tested_value.z in
    let cm_w = cond_matches_scalar cond tested_value.w in
    if cond.c_and
    then
	((not cond.c_x) || cm_x) &&
	((not cond.c_y) || cm_y) &&
	((not cond.c_z) || cm_z) &&
	((not cond.c_w) || cm_w)
    else
	(cond.c_x && cm_x) ||
	(cond.c_y && cm_y) ||
	(cond.c_z && cm_z) ||
	(cond.c_w && cm_w)

let rec vm_execute_op : vm -> op -> bool
= fun vm -> function
| #op_arith as op -> let (wb,computed_value) = op_arith_compute vm op in
                         (
                         vm_writeback vm wb computed_value;
			 true;
			 )
|`COND(a,cond,ci) ->
               let (wb, computed_value) = op_arith_compute vm a in
               (
	          vm_writeback vm wb computed_value;
                  if cond_matches cond computed_value
		      then vm_execute_op vm (ci :> op)
		      else true
	       )
|`JMP(i) -> (* FIXME: is it supposed to be an absolute or relative jump ? *)
	(
	vm.pc <- i;
	true
	)
|`LOAD(i,a,ashf) ->
	let addr = (addr_geti vm a) + ashf in
	(
	vm.regs.(reg_index_Ix+i) <- memory_fetch_reg vm.data_mem addr;
	true;
	)
|`LOAD4(a,ashf) ->
	let addr = (addr_geti vm a) + ashf in
	(
	vm.regs.(reg_index_I0) <- memory_fetch_reg vm.data_mem (addr);
	vm.regs.(reg_index_I1) <- memory_fetch_reg vm.data_mem (addr+4);
	vm.regs.(reg_index_I2) <- memory_fetch_reg vm.data_mem (addr+8);
	vm.regs.(reg_index_I3) <- memory_fetch_reg vm.data_mem (addr+12);
	true;
	)
|`STORE(a,ashf,v) ->
	let addr = (addr_geti vm a) + ashf in
	(
	memory_store_reg vm.data_mem addr (vm_fetch vm v);
	true;
	)
|`RETURN ->
	    if stack_return_ends_shader vm.stack
	    then false
	    else (
                vm.pc <- stack_end vm.stack;
                true
            )
|`TEXLOAD(i,txd,coords) ->
    let v = vm_texload vm (vm_fetch vm txd) (vm_fetch vm coords) in 
    (
    vm.regs.(reg_index_Ix+i) <- v;
    true;
    )
|`TEXLOAD4(txd,coords) ->
    let (v0,v1,v2,v3) = vm_texload4 vm (vm_fetch vm txd) (vm_fetch vm coords) in
    (
    vm.regs.(reg_index_I0) <- v0;
    vm.regs.(reg_index_I1) <- v1;
    vm.regs.(reg_index_I2) <- v2;
    vm.regs.(reg_index_I3) <- v3;
    true;
    )
|`TRACE(orig, dir, spec) ->
	(
	let orig = reg_to_v3 (vm_fetch vm orig) in
	let dir  = reg_to_v3 (vm_fetch vm dir) in
	let spec = vm_fetch vm spec in
	(
	match Hardware.trace orig dir (spec.x, spec.y) vm.data_mem 0
	with
	| Some(hit_dist, tri_addr, u, v) ->
	    (
                (* w - shader addr, due to lack of support in scene preprocesser,
                       always make it 0 *)
		vm.regs.(reg_index_HIT) <- {x=u; y=v; z=hit_dist; w=0.0};
		vm.triaddr <- float_of_int tri_addr;
		vm.objaddr <- float_of_int tri_addr;
	    )
	| None ->
	    (
		vm.regs.(reg_index_HIT) <- {x=0.0; y=0.0; z=neg_infinity; w=0.0};
		vm.triaddr <- 0.0;
		vm.objaddr <- 0.0;
	    )
	);
	true;
	)
|`CALL(call_addr, push_size) ->
    (
        stack_begin vm.stack vm.pc push_size;
        vm.pc <- call_addr;
	true
    )
|`DCALL(push_size) ->
    (
        let call_addr = int_of_float (vm.regs.(reg_index_HIT).w +. 0.5) in
        stack_begin vm.stack vm.pc push_size;
        vm.pc <- call_addr;
	true
    )
let vm_step : vm -> bool
= fun vm ->
    let op = DynArray.get vm.code_mem vm.pc in
    (
	vm.pc <- vm.pc + 1;
        vm_execute_op vm op
    )

let vm_run_shader vm =
    let rec loop () =
        (*printf "PC: %d\n" vm.pc;*)
	if vm_step vm
	then
	    loop()
in
    loop()

let vm_print_regs vm =
    for i = 0 to (reg_count - 1) do
        let name = Opcodes.register_to_string i in
        let r = vm.regs.(i) in
        printf "%s.x = %f\n" name r.x;
        printf "%s.y = %f\n" name r.y;
        printf "%s.z = %f\n" name r.z;
        printf "%s.w = %f\n" name r.w
    done
