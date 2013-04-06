open Printf
open Shader

let x = 0
let y = 1
let z = 2
let w = 3

type vm = {shader: shader; regs: float array array; mutable pc: int }
let vm_new shader = {shader=shader; regs=Array.create 17 [|0.0; 0.0; 0.0; 1.0|]; pc=0}

let sprintf_vec v = sprintf "<%f %f %f %f>" v.(x) v.(y) v.(z) v.(w)

(* vm is a read-write argument
 * Return false if it's over, true if it's ok to run some more
 *)
let vm_run_step vm =
    let ((op,imod,(target,wbm),sources),sec) = vm.shader.(vm.pc)
 in let swizzle v (xsw,ysw,zsw,wsw) = [|v.(xsw); v.(ysw); v.(zsw); v.(wsw)|]
 in let sources = List.map (function
	    | SrcImm v          -> [|v;v;v;v|]
	    | SrcReg(r,sw,amod) -> let v = vm.regs.(r)
				in let v = swizzle v sw
				in Array.map (fun v -> amod *. v) v
	) sources
 in let frac v = v -. (floor v)
 in let res = match (op,sources) with
	| (MOV,[v])        -> v
	| (FRAC,[v])       -> [|frac v.(x); frac v.(y); frac v.(z); frac v.(w); |]
	| (ADD,[v1;v2])    -> [|v1.(x)+.v2.(x); v1.(y)+.v2.(y); v1.(z)+.v2.(z); v1.(w)+.v2.(w);|]
	| (MUL,[v1;v2])    -> [|v1.(x)*.v2.(x); v1.(y)*.v2.(y); v1.(z)*.v2.(z); v1.(w)*.v2.(w);|]
	| (MAD,[v1;v2;v3]) -> [|v1.(x)*.v2.(x)+.v3.(x); v1.(y)*.v2.(y)+.v3.(y);
				v1.(z)*.v2.(z)+.v3.(z);	v1.(w)*.v2.(w)+.v3.(w)|]
	| (DP3,[v1;v2])    -> let v = v1.(x)*.v2.(x) +. v1.(y)*.v2.(y) +. v1.(z)*.v2.(z)
			      in [| v; v; v; v |]
	| (DP2H,[v1;v2])   -> let v = v1.(x)*.v2.(x) +. v1.(y)*.v2.(y) +. v1.(z)
			      in [| v; v; v; v |]
	| (DP4,[v1;v2])    -> let v = v1.(x)*.v2.(x) +. v1.(y)*.v2.(y) +. v1.(z)*.v2.(z) +. v1.(w)*.v2.(w)
			      in [| v; v; v; v |]
	| (DP3H,[v1;v2])   -> let v = v1.(x)*.v2.(x) +. v1.(y)*.v2.(y) +. v1.(z)*.v2.(z) +. v1.(w)
			      in [| v; v; v; v |]
	
	| (MOV,s) -> failwith (sprintf "Internal error: Wrong number of source arguments for MOV: %d passed, 1 expected" (List.length s))
	| (FRAC,s)-> failwith (sprintf "Internal error: Wrong number of source arguments for FRAC: %d passed, 1 expected" (List.length s))
	| (ADD,s) -> failwith (sprintf "Internal error: Wrong number of source arguments for ADD: %d passed, 2 expected" (List.length s))
	| (MUL,s) -> failwith (sprintf "Internal error: Wrong number of source arguments for MUL: %d passed, 2 expected" (List.length s))
	| (MAD,s) -> failwith (sprintf "Internal error: Wrong number of source arguments for MAD: %d passed, 3 expected" (List.length s))
	| (DP3,s) -> failwith (sprintf "Internal error: Wrong number of source arguments for DP3: %d passed, 2 expected" (List.length s))
	| (DP2H,s)-> failwith (sprintf "Internal error: Wrong number of source arguments for DP2H: %d passed, 2 expected" (List.length s))
	| (DP4,s) -> failwith (sprintf "Internal error: Wrong number of source arguments for DP4: %d passed, 2 expected" (List.length s))
	| (DP3H,s)-> failwith (sprintf "Internal error: Wrong number of source arguments for DP3H: %d passed, 2 expected" (List.length s))
 in let clamp_scalar v = if v < 0.0 then 0.0 else if v > 1.0 then 1.0 else v
 in let clamp = Array.map clamp_scalar
 in let rcp_scalar v = 1.0 /. v
 in let rcp = Array.map rcp_scalar
 in let rsq v = let rsq_val = 1.0 /. sqrt(v.(w)) in [| rsq_val; rsq_val; rsq_val; rsq_val |]
 in let (res,s) = match imod with
	| None     -> (res,None)
	| Some SAT -> (clamp res,None)
	| Some RCP -> (res,Some (rcp res))
	| Some RSQ -> (res,Some (rsq res))
 in let write_back t (xwb,ywb,zwb,wwb) s =
	    (
		if xwb then vm.regs.(t).(x) <- s.(x);
		if ywb then vm.regs.(t).(y) <- s.(y);
		if zwb then vm.regs.(t).(z) <- s.(z);
		if wwb then vm.regs.(t).(w) <- s.(w);
	    )
 in let _ = (match s with None -> () | Some s -> write_back 16 wbm s)
 in let _ = write_back target wbm res
 in let cond_eval ((lt0,b01,ge1),(xm,ym,zm,wm),r) v =
	let cond_eval_scalar v = (if v<0.0 then lt0 else if v >= 1.0 then ge1 else b01)
     in let (cx,cy,cz,cw) = (cond_eval_scalar v.(x), cond_eval_scalar v.(y), cond_eval_scalar v.(z), cond_eval_scalar v.(w))
     in match r with
       | RED_OR  -> ((xm && cx) || (ym && cy) || (zm && cz) || (wm && cw))
       | RED_AND -> ((xm || cx) && (ym || cy) && (zm || cz) && (wm || cw))
 in let _ = vm.pc <- vm.pc+1
 in match sec with
    | None -> true
    | Some(CJMP(cond,jmp_to)) -> ((if (cond_eval cond res) then vm.pc <- jmp_to else ()); true)
    | Some(RETURN cond) -> not(cond_eval cond res)

let rec vm_run vm = if vm_run_step vm then vm_run vm else ()

let build_vm shader = {
    shader = shader;
    regs = [| 
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R0 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R1 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R2 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R3 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R4 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R5 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R6 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R7 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R8 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R9 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R10*)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R11*)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R12*)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R13 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R14 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* R15 *)
	[| 0.0; 0.0; 0.0; 1.0 |]; (* S *)
    |];
    pc = 0
}
;;

let (shader_depth : shader) = input_value (open_in "depth.shr")

let vm_new shader_file = build_vm (input_value (open_in shader_file))

let shade_pixel vm u v depth ray_origin ray_dir hp a b c tri_color light =
 (
    vm.regs.(0).(x) <- u;
    vm.regs.(0).(y) <- v;
    vm.regs.(0).(z) <- depth;
    vm.regs.(0).(w) <- 1.0;
    vm.regs.(1)     <- ray_origin;
    vm.regs.(2)     <- ray_dir;
    vm.regs.(3)     <- hp;
    vm.regs.(4)     <- a;
    vm.regs.(5)     <- b;
    vm.regs.(6)     <- c;
    vm.regs.(7)     <- tri_color;
    vm.regs.(8)     <- light;
    vm.pc <- 0;
    (* The value of other registers is undefined :-D
       (actually it should be defined if the debugger is to work correctly) *)
    vm_run vm;
    (vm.regs.(15).(x),vm.regs.(15).(y),vm.regs.(15).(z))
 )

let reg_name r =
    if r >= 0 && r <= 15
    then sprintf "R%d" r
    else if r = 16
    then "S"
    else failwith (sprintf "Only registers 0 to 16 supported, not %d" r)

let reg_to_string r =
    sprintf "[%g; %g; %g; %g]" r.(0) r.(1) r.(2) r.(3)

let print_regs regs =
    for i = 0 to 16 do
	printf "%s = %s\n" (reg_name i) (reg_to_string regs.(i))
    done

let rec vm_run_trace vm =
    let old_regs = Array.map (Array.map (fun x->x)) vm.regs
 in let instr_txt = Shader_print.sprintf_inst vm.pc vm.shader.(vm.pc)
 in let instr_txt = match instr_txt with
		  | [(Some i,a)] -> sprintf "Instruction %d: %s\n" i a;
		  | [(Some i,a);(None,b)] -> sprintf "Instruction %d: %s\n                %s\n" i a b;
		  | _ -> failwith "Weird instruction format from Shader_print.sprintf_inst"
 in
 (
    printf "%s" instr_txt;
    let keep_running = vm_run_step vm
 in for i = 0 to 16 do
        if vm.regs.(i) <> old_regs.(i)
	then printf "%s = %s\n" (reg_name i) (reg_to_string vm.regs.(i))
    done;
    if keep_running
    then vm_run_trace vm
    else printf "END\n"
 )

let shade_pixel_trace vm u v depth ray_origin ray_dir hp a b c tri_color light =
 (
    vm.regs.(0).(x) <- u;
    vm.regs.(0).(y) <- v;
    vm.regs.(0).(z) <- depth;
    vm.regs.(0).(w) <- 1.0;
    vm.regs.(1)     <- ray_origin;
    vm.regs.(2)     <- ray_dir;
    vm.regs.(3)     <- hp;
    vm.regs.(4)     <- a;
    vm.regs.(5)     <- b;
    vm.regs.(6)     <- c;
    vm.regs.(7)     <- tri_color;
    vm.regs.(8)     <- light;
    vm.pc <- 0;
    
    (* For tracing define the other registers
     * Warning: Heisenbugs possible !
     *)
    vm.regs.(9) <- [| 0.0; 0.0; 0.0; 1.0 |];
    vm.regs.(10) <- [| 0.0; 0.0; 0.0; 1.0 |];
    vm.regs.(11) <- [| 0.0; 0.0; 0.0; 1.0 |];
    vm.regs.(12) <- [| 0.0; 0.0; 0.0; 1.0 |];
    vm.regs.(13) <- [| 0.0; 0.0; 0.0; 1.0 |];
    vm.regs.(14) <- [| 0.0; 0.0; 0.0; 1.0 |];
    vm.regs.(15) <- [| 0.0; 0.0; 0.0; 1.0 |];
    vm.regs.(16) <- [| 0.0; 0.0; 0.0; 1.0 |];

    print_regs vm.regs;
    printf "\n";
    vm_run_trace vm;
    (vm.regs.(15).(x),vm.regs.(15).(y),vm.regs.(15).(z))
 )
