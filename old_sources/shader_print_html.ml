open Shader
open Printf

let reg_name r =
    if r >= 0 && r <= 15
    then sprintf "R%d" r
    else if r = 16
    then "S"
    else failwith (sprintf "Only registers 0 to 16 supported, not %d" r)

(*
 * Module for HTML tables
 * Each cell has type (int*int*string*string) - colspan * rowspan * extra_attributes * contents
 * If the cell is missing, pass (0, 0, "")
 *)
let empty_cell                   = (0,0,"","")
let simple_cell classes contents = (1,1,(if classes = "" then "" else sprintf " class='%s'" classes),contents)
let wide_cell m classes contents = (1,m,(if classes = "" then "" else sprintf " class='%s'" classes),contents)
let tall_cell n classes contents = (n,1,(if classes = "" then "" else sprintf " class='%s'" classes),contents)

let render_cell (n,m,attrs,contents) =
    if n=0 || m=0
    then ""
    else
	sprintf "<td%s%s%s>%s</td>"
	(if n=1 then "" else (sprintf " rowspan='%d'" n))
	(if m=1 then "" else (sprintf " colspan='%d'" m))
	attrs
	contents

let layout_row row = (List.fold_left (fun x y -> x^render_cell y) "<tr>" row)^"</tr>\n"

(*
let lol_foldii f p0 a =
    let lol_foldii_aux (major,minor) p = function
    | []            -> ()
    | []::tl        -> lol_foldii (major+1,0) p tl
    | (hd::tl)::tl2 -> let p2 = f major minor p hd in lol_foldii_aux (major,minor+1) p2 (tl::tl2)
 in lol_foldii_aux (0,0) p0 a

let layout_cols cols =
    let (maxx,maxy) = lol_foldii (fun colnum rownum (mxr,mxc) (rs,cs,_,_) -> ((max mxr (rownum+rs)), (max mxc (colnum+mxc)))) (0,0) cols
 in let a = Array.create_matrix maxx maxy None
 in failwith "Not implemented yet"
*)

(*
 * It only works if there is no cell overlap (like the standard says)
 *)
let layout_cols cols =
    let cols_a = Array.of_list cols
 in let cells = Hashtbl.create 0 (* (row,col) -> code *)
 in let rec skip_rows c r =
	if Hashtbl.mem cells (r,c)
	then skip_rows c (r+1)
	else r
 in let add_cell rnum cnum rsp csp cell_code =
	for c = cnum to (cnum+csp-1) do
	    for r = rnum to (rnum+rsp-1) do
		if Hashtbl.mem cells (r,c)
		then failwith (sprintf "Table cell (%d,%d) is already occupied" r c)
		else 
(*		    printf "Added cell (%d,%d)=\"%s\"%s\n" r c cell_code (if r=rnum && c=cnum then "" else " (shadow)"); *)
		    Hashtbl.replace cells (r,c) (if r=rnum && c=cnum then cell_code else "")
	    done
	done
 in let _ =
    for col_num = 0 to (Array.length cols_a - 1)
    do
	let row_num = ref 0 in
	let col = Array.of_list (cols_a.(col_num)) in
	for i = 0 to (Array.length col - 1)
	do
	    row_num := skip_rows col_num !row_num;
	    let cell = col.(i) in
	    let (rsp,csp,_,_) = cell in
	    let cell_code = render_cell cell in
	    add_cell !row_num col_num rsp csp cell_code;
	    row_num := !row_num + rsp
	done    
    done
 (* No autovivification in Ocaml, need to do it the hard way
  *)
 in let rows = Hashtbl.create 0 (* row -> (col*code) list *)
 in let _ = Hashtbl.iter (fun (r,c) code ->
(*			    printf "Cell (%d,%d): %s\n" r c code; *)
			    Hashtbl.replace rows r ((c,code)::(
			    try Hashtbl.find rows r
			    with Not_found -> []
			  ))) cells
 in let rows_code = ref []
 in let _ = Hashtbl.iter (fun r codes ->
       let sorted = List.fast_sort (fun (c1,code1) (c2,code2) -> c1-c2) codes
    in let row = (List.fold_left (fun x (_,y) -> x^y) "<tr>" sorted) ^ "</tr>\n"
(*    in let _ = printf "Row %d: %s\n" r row *)
    in rows_code := (r,row)::!rows_code
 ) rows
 in let sorted =  List.fast_sort (fun (r1,code1) (r2,code2) -> r1-r2) (!rows_code)
 in List.fold_left (fun x (_,y) -> x^y) "" sorted

(*
 * Possible columns (without shaded IGNORE column):
 * 1-cell     (no sw && no smod) 
 * 3-cell     (external swizzle && no smod)
 * 3-cell     (no swizzle       && external smod)
 * 5-cell     (external swizzle && external smod)
 * 1+1-cell   (internal swizzle && no smod)
 * 1+1-cell   (no swizzle       && internal smod)
 * 1+3-cell   (no swizzle       && external smod)
 * 3+1-cell   (external swizzle && internal smod)
 * 1+1+1-cell (internal swizzle && internal smod)
 *)
let convert_src_reg r sw (smod) (force_sw,force_smod) (nx,ny,nz,nw) =
    let (swx,swy,swz,sww) = sw
 in let n1 = [|nx;ny;nz;nw|]
 in let n0 = [|false;false;false;false|]
 in let _ = (
	    n0.(swx) <- n0.(swx) || nx;
	    n0.(swy) <- n0.(swy) || ny;
	    n0.(swz) <- n0.(swz) || nz;
	    n0.(sww) <- n0.(sww) || nw;
	    )
 in let dc = function true -> "" | false -> "deadcode"
 in let dcsw3 n0 n1 = match (n0,n1) with (true,true) -> "" | (true,false) -> "swizzle_and_die" | (false,_) -> "deadcode"
 in let row0                      = wide_cell 4 "reg_src" (reg_name r)
 in let cell_1     cn sw sm x _ _ = [simple_cell (dc n0.(cn)) (sprintf "%g" x)]
 in let cell_3     cn sw sm x _ _ = [tall_cell 3 (dc n0.(cn)) (sprintf "%g" x)]
 in let cell_5     cn sw sm x _ _ = [tall_cell 5 (dc n0.(cn)) (sprintf "%g" x)]
 in let cell_1_1sw cn sw sm x y _ = [simple_cell (dc n0.(cn)) (sprintf "%g" x)]@
			             sw@
			    	    [simple_cell (dc n1.(cn)) (sprintf "%g" y)]
 in let cell_1_1sm cn sw sm x _ z = [simple_cell (dc n0.(cn)) (sprintf "%g" x)]@
				     sm@
			    	    [simple_cell (dc n1.(cn)) (sprintf "%g" z)]
 in let cell_1_3   cn sw sm x y _ = [simple_cell (dc n0.(cn)) (sprintf "%g" x)]@
				     sw@
				    [tall_cell 3 (dc n1.(cn)) (sprintf "%g" y)]
 in let cell_3_1   cn sw sm x _ z = [tall_cell 3 (dcsw3 n0.(cn) n1.(cn)) (sprintf "%g" x)]@
			    	     sm@
			    	    [simple_cell (dc n1.(cn)) (sprintf "%g" z)]
 in let cell_1_1_1 cn sw sm x y z = [simple_cell (dc n0.(cn)) (sprintf "%g" x)]@
				     sw@
				    [simple_cell (dc n1.(cn)) (sprintf "%g" y)]@
			    	     sm@
				    [simple_cell (dc n1.(cn)) (sprintf "%g" z)]
 in let gen_column cn csw =
    match (cn = csw,force_sw,smod = 1.0,force_smod) with
    | ( true,false, true,false) -> cell_1 cn 
    | ( true, true, true,false) -> cell_3 cn 
    | ( true,false, true, true) -> cell_3 cn 
    | ( true, true, true, true) -> cell_5 cn 
    | (false,    _, true,false) -> cell_1_1sw cn 
    | ( true,false,false,    _) -> cell_1_1sm cn 
    | (false,    _, true, true) -> cell_1_3 cn 
    | ( true, true,false,    _) -> cell_3_1 cn 
    | (false,    _,false,    _) -> cell_1_1_1 cn 
 in fun v ->
       let vx = v.(r).(swx)*.smod
    in let vy = v.(r).(swy)*.smod
    in let vw = v.(r).(swz)*.smod
    in let vz = v.(r).(sww)*.smod
    in let sm_cell = if smod = 1.0
		     then []
		     else [wide_cell 4 "" (sprintf "&times; %g" smod)]
    in let sw_cell_x = if swx = 0 then [] else [simple_cell (dc n1.(0)) [|"x";"y";"z";"w"|].(swx)]
    in let sw_cell_y = if swy = 1 then [] else [simple_cell (dc n1.(1)) [|"x";"y";"z";"w"|].(swy)]
    in let sw_cell_z = if swz = 2 then [] else [simple_cell (dc n1.(2)) [|"x";"y";"z";"w"|].(swz)]
    in let sw_cell_w = if sww = 3 then [] else [simple_cell (dc n1.(3)) [|"x";"y";"z";"w"|].(sww)]
    in let col_x = (gen_column 0 swx) sw_cell_x sm_cell v.(r).(0) v.(r).(swx) vx 
    in let col_y = (gen_column 1 swy) sw_cell_y []      v.(r).(1) v.(r).(swy) vy
    in let col_z = (gen_column 2 swz) sw_cell_z []      v.(r).(2) v.(r).(swz) vz
    in let col_w = (gen_column 3 sww) sw_cell_w []      v.(r).(3) v.(r).(sww) vw
    in (row0,[col_x;col_y;col_z;col_w],(vx,vy,vw,vz))


let space_for_swizzle_needed = function
    | SrcImm _ -> false
    | SrcReg(_,sw,_) -> sw <> (0,1,2,3)

let space_for_smod_needed = function
    | SrcImm _ -> false
    | SrcReg(_,_,smod) -> smod <> 1.0

let convert_src (s,needed) (force_sw,force_smod) = match s with
(* v is some kind of a state argument - float array array *)
| SrcImm x -> fun _ -> let r0 = wide_cell 4 "imm_src" (sprintf "%g" x)
		       in let (nx,ny,nz,nw) = needed
		       in let r1c n = tall_cell 
			    (1+(if force_sw then 2 else 0)+(if force_smod then 2 else 0))
			    (if n then "" else "deadcode")
			    (sprintf "%g" x)
		       in (r0,[[r1c nx];[r1c ny];[r1c nz];[r1c nw]],(x,x,x,x))
| SrcReg(r,sw,sm) -> convert_src_reg r sw sm (force_sw,force_smod) needed

(*
let convert_src1 (s1,needed1) =
    let force_sw   = space_for_swizzle_needed s1
 in let force_smod = space_for_smod_needed s1
 in let src1       = convert_src (s1,needed1) (force_sw,force_smod)
 in let sz         = (2+(if force_sw then 2 else 0)+(if force_smod then 2 else 0))
 in fun v ->
       let (r0,r1,r2,r3,r4,r5,v1) = src1 v
    in ("<tr>"^(margin_left ^ r0 ^ margin_right)^"</tr>\n"^
	(if force_sw   then "<tr>"^r2^"</tr>\n<tr>"^r3^"</tr>\n" else "")^
	(if force_smod then "<tr>"^r4^"</tr>\n<tr>"^r5^"</tr>\n" else "")
       ),v1
*)

let convert_src2 (s1,needed1) (s2,needed2) =
    let force_sw   = space_for_swizzle_needed s1 || space_for_swizzle_needed s2
 in let force_smod = space_for_smod_needed s1 || space_for_smod_needed s2
 in let src1       = convert_src (s1,needed1) (force_sw,force_smod)
 in let src2       = convert_src (s2,needed2) (force_sw,force_smod)
 in let sz         = (2+(if force_sw then 2 else 0)+(if force_smod then 2 else 0))
 in let margin_mid   = tall_cell sz "umspace" ""
 in fun v ->
       let (r0_1,cols_1,v1) = src1 v
    in let (r0_2,cols_2,v2) = src2 v
    in ((layout_row [r0_1;margin_mid;r0_2])^(layout_cols (cols_1@cols_2)),v1,v2)

(*
let convert_src3 s1 s2 s3 =
    let space_for_swizzle = space_for_swizzle_needed s1 || space_for_swizzle_needed s2 || space_for_swizzle_needed s3
 in let space_for_smod    = space_for_smod_needed s1 || space_for_smod_needed s2 || space_for_smod_needed s3
 in let src1              = convert_src s1 (space_for_swizzle,space_for_smod)
 in let src2              = convert_src s2 (space_for_swizzle,space_for_smod)
 in let src3              = convert_src s3 (space_for_swizzle,space_for_smod)
 in failwith "Not implemented"
*)

let convert_wb r (wbx,wby,wbz,wbw) (x,y,z,w) =
    let kl m = if m then "" else " class='masked_wb'"
 in let sm m = if m then "&darr;" else "-"
 in let r1 = sprintf  "<td%s>%g</td><td%s>%g</td><td%s>%g</td><td%s>%g</td>"
		      (kl wbx) x (kl wby) y (kl wbz) z (kl wbw) w
 in let r2 = sprintf  "<td%s>%s</td><td%s>%s</td><td%s>%s</td><td%s>%s</td>"
		      (kl wbx) (sm wbx) (kl wby) (sm wby) (kl wbz) (sm wbz) (kl wbw) (sm wbw)
 in let r3 = sprintf "<td colspan='4' class='reg_dst'>%s</td>" (reg_name r)
 in (r1, r2, r3)

(* 
 * backpropagate_need_* and propagate_val_* are to be used for building op printers
 *)
let backpropagate_need_mov  n = n
let backpropagate_need_frac n = n
let backpropagate_need_add  n = (n,n)
let backpropagate_need_mul  n = (n,n)
let backpropagate_need_mad  n = (n,n,n)
let backpropagate_need_dp3 (nx,ny,nz,nw) =  let n = (nx||ny||nz||nw) in ((n,n,n,false),(n,n,n,false))
let backpropagate_need_dp2h(nx,ny,nz,nw) =  let n = (nx||ny||nz||nw) in ((n,n,n,false),(n,n,false,false))
let backpropagate_need_dp4 (nx,ny,nz,nw) =  let n = (nx||ny||nz||nw) in ((n,n,n,n),(n,n,n,n))
let backpropagate_need_dp3h(nx,ny,nz,nw) =  let n = (nx||ny||nz||nw) in ((n,n,n,n),(n,n,n,false))

let propagate_val_mov v = v
let propagate_val_frac(vx,vy,vz,vw) = let fr v = v-.floor v in (fr vx,fr vy,fr vz,fr vw)
let propagate_val_add (v1x,v1y,v1z,v1w) (v2x,v2y,v2z,v2w) = (v1x+.v2x,v1y+.v2y,v1z+.v2z,v1w+.v2w)
let propagate_val_mul (v1x,v1y,v1z,v1w) (v2x,v2y,v2z,v2w) = (v1x*.v2x,v1y*.v2y,v1z*.v2z,v1w*.v2w)
let propagate_val_mad (v1x,v1y,v1z,v1w) (v2x,v2y,v2z,v2w) (v3x,v3y,v3z,v3w) =
		      (v1x*.v2x+.v3x, v1y*.v2y+.v3y, v1z*.v2z+.v3z, v1w*.v2w+.v3w)
let propagate_val_dp3 (v1x,v1y,v1z,_) (v2x,v2y,v2z,_) =
		      let v = v1x*.v2x +. v1y*.v2y +. v1z*.v2z
		      in (v,v,v,v)
let propagate_val_dp2h(v1x,v1y,v1z,_) (v2x,v2y,_,_) =
		      let v = v1x*.v2x +. v1y*.v2y +. v1z
		      in (v,v,v,v)
let propagate_val_dp4 (v1x,v1y,v1z,v1w) (v2x,v2y,v2z,v2w) =
		      let v = v1x*.v2x +. v1y*.v2y +. v1z*.v2z +. v1w*.v2w
		      in (v,v,v,v)
let propagate_val_dp3h (v1x,v1y,v1z,v1w) (v2x,v2y,v2z,_) =
		      let v = v1x*.v2x +. v1y*.v2y +. v1z*.v2z +. v1w
		      in (v,v,v,v)

let convert_a2 back forw op_name =
    fun (imod : imod option) (target_reg,writeback_mask) sources extra_needed ->
    (*  compute depending on OP, writeback_mask and cond *)
    let (s1,s2) = (match sources with
		  | [s1;s2] -> (s1,s2)
		  | _ -> failwith (sprintf "%s expects 2 source arguments, %d given" op_name (List.length sources)))
 in let (needed_1,needed_2) = back writeback_mask
 in let c  = convert_src2 (s1,needed_1) (s2,needed_2)
 in let margin_left =  sprintf "<td class='lwspace' rowspan='%d'>" 3
 in let margin_right =  sprintf "<td class='rwspace' rowspan='%d'>" 3
 (* Needs regs at some point *)
 in fun regs -> 
    let (res,v1,v2) = c regs
 in let ((vx,vy,vz,vw) as op_res_val) = propagate_val_add v1 v2
 in let (wbx,wby,wbz,wbw) = writeback_mask
 in let (r1,r2,r3) = convert_wb target_reg writeback_mask op_res_val
 in let wb_stage = ("<tr>"^margin_left^r1^margin_right^"</tr>\n")^
		   ("<tr>"^r2^"</tr>\n")^
		   ("<tr>"^r3^"</tr>\n")
 in let res = (sprintf "<div class='instr'><table class='source'>
 <colgroup span='4' width='11.875%%'>
 <colgroup span='1' width='5%%'>
 <colgroup span='4' width='11.875%%'>
 %s
 </table>
 <table><tr><td class='op'>%s</td></tr></table>
 <table class='writeback'>
 <colgroup span='1' width='26.25%%'>
 <colgroup span='4' width='11.875%%'>
 <colgroup span='1' width='26.25%%'>
 %s</table>
 </div>" res op_name wb_stage)
 in let exec_op regs2 =
    (
	if wbx then regs2.(target_reg).(0) <- vx;
	if wby then regs2.(target_reg).(1) <- vy;
	if wbz then regs2.(target_reg).(2) <- vz;
	if wbw then regs2.(target_reg).(3) <- vw;
    )
 in (res,exec_op)

let convert_mov  _ _ _ _ _ = "<h1>convert_mov implemented yet</h1>", (fun _->())
let convert_frac _ _ _ _ _ = "<h1>convert_frac implemented yet</h1>", (fun _->())
let convert_add    = convert_a2 backpropagate_need_add  propagate_val_add  "ADD"
let convert_mul    = convert_a2 backpropagate_need_mul  propagate_val_mul  "MUL"
let convert_mad _ _ _ _ _ = "<h1>convert_mad implemented yet</h1>", (fun _->())
let convert_dp3    = convert_a2 backpropagate_need_dp3  propagate_val_dp3  "DP3"
let convert_dp2h   = convert_a2 backpropagate_need_dp2h propagate_val_dp2h "DP2H"
let convert_dp4    = convert_a2 backpropagate_need_dp4  propagate_val_dp4  "DP4"
let convert_dp3h   = convert_a2 backpropagate_need_dp3h propagate_val_dp3h "DP3H"

let (shader_to_print : shader) = input_value (open_in "pseudophong.shr")
(*
[|
 (
 (ADD,Some SAT,(15,(false,true,false,false)),[SrcImm 1.8; SrcReg(0,(2,2,2,2),-1.0)]),
  None
 );
 (
  (ADD,Some SAT,(15,(true,false,true,false)),[SrcImm 1.0; SrcReg(15,(1,1,1,1),-1.0)]),
   Some(RETURN((true,true,true),(true,true,true,true),RED_OR))
 )
|]
*)

(*
 * In case of unconditional jumps, don't introduce a fake dependency
 *)
let cond_need (c0,m, _) =
    if c0 = (true,true,true) || c0 = (false,false,false)
    then (false,false,false,false)
    else m 

let sec_need = function
| CJMP(c,label)-> cond_need c
| RETURN(c)    -> cond_need c

let arith_to_instr (op,imod,target,sources) xneed =
    let cv = match op with
    | MOV  -> convert_mov
    | FRAC -> convert_frac
    | ADD  -> convert_add
    | MUL  -> convert_mul
    | MAD  -> convert_mad
    | DP3  -> convert_dp3
    | DP2H -> convert_dp2h
    | DP4  -> convert_dp4
    | DP3H -> convert_dp3h
 in let c = cv imod target sources xneed
 in c

(* Secondary instruction is not rendered *)
let instr_to_html (arith, sec) =
    let ai = arith_to_instr arith (match sec with None -> (false,false,false,false) | Some sec -> sec_need sec)
 in ai

(*
 * Assumes only trivial forward control
 *)
let shader_to_html shader regs =
    let (renderings : string array) = Array.mapi (fun i instr ->
	let c = instr_to_html instr
     in let ((rendering, instr_exec) : (string * (float array array -> unit))) = c regs
     in (instr_exec regs; rendering)
    ) shader
    in Array.fold_left (fun x y -> x^y) "" renderings

let doc_content =
  let regs = [|
    [|0.0; 8.0; 1.2; 1.0|]; (* R0 *)
    [|0.1; 0.7; 4.0; 1.0|]; (* R1 *)
    [|0.2; 6.0; 4.4; 1.0|]; (* R2 *)
    [|0.3; 5.0; 3.3; 1.0|]; (* R3 *)
    [|0.4; 4.0; 2.2; 1.0|]; (* R4 *)
    [|0.5; 0.3; 1.1; 1.0|]; (* R5 *)
    [|0.6; 0.2; 9.9; 1.0|]; (* R6 *)
    [|0.7; 1.0; 8.8; 1.0|]; (* R7 *)
    [|0.8; 0.9; 7.7; 1.0|]; (* R8 *)
    [|0.9; 8.0; 6.6; 1.0|]; (* R9 *)
    [|0.8; 0.7; 5.5; 1.0|]; (* R10 *)
    [|0.7; 0.6; 4.4; 1.0|]; (* R11 *)
    [|0.6; 5.0; 3.3; 1.0|]; (* R12 *)
    [|0.5; 4.0; 2.2; 1.0|]; (* R13 *)
    [|0.4; 3.0; 1.1; 1.0|]; (* R14 *)
    [|0.3; 2.0; 2.2; 1.0|]; (* R15 *)
    [|0.2; 1.0; 3.0; 1.0|]; (* S *)
  |]
  in shader_to_html shader_to_print regs

(*
    let c = convert_add (None) (9,(true,true,true,false)) [SrcReg(8,(0,1,2,2),1.0);SrcReg(3,(0,1,2,2),-1.0)]
 in failwith ""
*)

(* style sheet should actually be external
 * 
 * The style sheet doesn't match the document at all
 *)
let style_sheet = "
table {
    border-collapse: collapse;
    width: 100%;
}
.instr {
    margin-bottom: 1em;
    width: 60%;
}
/* This is a default for all TDs */
td {
    text-align: center;
    vertical-align: bottom;
    border-color: lightgray gray;
    padding: 0px;
    margin: 0px;
    border-width: 0px;
    border-style: solid;
}
.op {
    font-size: 200%;
    font-family: sans-serif;
    border-width: 0px 5px;
    border-style: none solid;
}
/* These two rules should really have a low priority*/
.source td {
    border-width: 1px 3px;
    padding: 2px;
}
.writeback td {
    border-width: 1px 3px;
    padding: 2px;
}
/* And these should have higher priority */
table td.imm_src { font-size: 150%; font-family: sans-serif; border-top-width: 5px; border-top-color: gray; }
table td.reg_src { font-size: 150%; font-family: sans-serif; border-top-width: 5px; border-top-color: gray; }
table td.reg_dst {
    font-size: 150%;
    font-family: sans-serif;
    border-bottom-width: 5px;
    border-bottom-color: gray;
}
table td.umspace {
    border-top-style: none;
    border-width: 0px 5px 5px 5px;
    border-bottom-color: gray;
}
table td.rwspace {
    border-bottom-style: none;
    border-right-style: none;
    border-left-width: 5px;
    border-top-width: 5px;
    border-top-color: gray;
}
table td.lwspace {
    border-bottom-style: none;
    border-right-width: 5px;
    border-left-style: none;
    border-top-width: 5px;
    border-top-color: gray;
}
table td.lborder { border-left-width: 5px; }
table td.rborder { border-right-width: 5px; }

table td.masked_wb {
    color: lightgray; /* only text color */
}
table td.swizzle_and_die  {
    vertical-align: top;
}
table td.deadcode {
    color: lightgray; /* only text color */
    vertical-align: top;
}
"

let doc_title = "A shader"
;;
printf "<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN' 'http://www.w3.org/TR/html4/loose.dtd'>
<html>
<head>
<title>%s</title>
<meta http-equiv='Content-type' content='text/html; charset=UTF-8'>
<style type='text/css'>
%s
</style>
</head>
<body>
%s
</body></html>
" doc_title style_sheet doc_content
