open Shader
open Printf

let sprintf_arith (op, imod, (target,wbm), sources) =
    let reg_name r =
	if r >= 0 && r <= 15
	then sprintf "R%d" r
	else if r = 16
	then "S"
	else failwith (sprintf "Only registers 0 to 16 supported, not %d" target)
 in let wbm_name (xm,ym,zm,wm) = 
    if (xm&&ym&&zm&&wm)
    then ""
    else
    "."^
    (if xm then "x" else "")^
    (if ym then "y" else "")^
    (if zm then "z" else "")^
    (if wm then "w" else "")
 in let sw_map s = [| "x"; "y"; "z"; "w" |].(s)
 in let sprintf_source (xm,ym,zm,wm) = function
   | SrcImm v -> sprintf "%g" v
   | SrcReg(reg,(sw0,sw1,sw2,sw3),smod) ->
   (
     match smod with
     |  0.5 -> "0.5*"
     |  1.0 -> ""
     |  2.0 -> "2*"
     |  4.0 -> "4*"
     | -0.5 -> "-0.5*"
     | -1.0 -> "-"
     | -2.0 -> "-2*"
     | -4.0 -> "-4*"
     | s    -> failwith (sprintf "Illegal source modifier %g" s)
   )^(reg_name reg)^
    (if (sw0,sw1,sw2,sw3) = (0,1,2,3)
    then ""
    (* simplify depending on the writeback mask maybe ? *)
    else
	"."^(sw_map sw0)^(sw_map sw1)^(sw_map sw2)^(sw_map sw3)
    )

 in (match op with
    | MOV  -> "mov"
    | FRAC -> "frac"
    | ADD  -> "add"
    | MUL  -> "mul"
    | MAD  -> "mad"
    | DP3  -> "dp3"
    | DP2H -> "dp2h"
    | DP4  -> "dp4"
    | DP3H -> "dp3h")^
    (match imod with
    | None -> ""
    | Some SAT -> "_sat"
    | Some RCP -> "_rcp"
    | Some RSQ -> "_rsq"
    )^" "^
    (reg_name target)^
    (wbm_name wbm)^
    (List.fold_left (fun a b -> a^", "^b) "" (List.map (sprintf_source wbm) sources))

let sprintf_cond (c0,(xm,ym,zm,wm),cond_reduction) = 
    (match cond_reduction with RED_OR -> "or" | RED_AND -> "and")^
    " "^
    (if xm then "x" else "")^
    (if ym then "y" else "")^
    (if zm then "z" else "")^
    (if wm then "w" else "")^
    (if xm||ym||zm||wm then "" else failwith "at least one of the mask elements should be true")^
    " "^
    (match c0 with
    | ( true, true, true) -> "(>=0 or <1)" (* always *)
    | ( true, true,false) -> "<1"
    | ( true,false, true) -> "(<0 or >=1)"
    | ( true,false,false) -> "<0"
    | (false, true, true) -> ">=0"
    | (false, true,false) -> "(>=0 and <1)"
    | (false,false, true) -> ">=1"
    | (false,false,false) -> "(<0 and >=1)" (* never *)
    )
let sprintf_sec = function
    | CJMP   (c,lab) -> (sprintf "+ jmp %d, " lab)^sprintf_cond(c)
    | RETURN (c)     -> "+ return "^sprintf_cond(c)

let sprintf_inst i = function
    | (arith, None)     -> [(Some(i), sprintf_arith arith)]
    | (arith, Some sec) -> [(Some(i), sprintf_arith arith); (None, sprintf_sec sec)]

let extract_important_labels shader  =
    let labels = Hashtbl.create 0
 in (
    for i = 0 to (Array.length shader - 1) do
	match shader.(i) with
	| (_,None)             -> ()
	| (_,Some CJMP(_,lab)) -> Hashtbl.replace labels lab ()
	| (_,Some RETURN(_))   -> ()
    done;
    labels
 )

let list_array_flatten_to_list x =
    let res = ref []
 in (
    for i = (Array.length x - 1) downto 0
    do
	res := x.(i) @ !res
    done;
    !res
    )

let print_shader shader =
    let printed_shader = list_array_flatten_to_list (Array.mapi sprintf_inst shader)
 in let extract_important_labels = extract_important_labels shader
 in let printed_shader = List.map (function
	| (None,instr) -> ("",instr)
	| (Some(label),instr) ->
	    if Hashtbl.mem extract_important_labels label
	    then ((sprintf "%d: " label),instr)
	    else ("",instr)
	) printed_shader
 in let adj_size = List.fold_left (fun x (label, _) -> max x (String.length label) ) 0 printed_shader
 in let printed_shader = List.map (fun (label, instr) ->
    sprintf "%-*s%s" adj_size label instr
 ) printed_shader
 in List.iter (fun s -> printf "%s\n" s) printed_shader
 
(*
print_shader (input_value stdin)
*)