(*
 * We assume the stuff does not overlap much
 *)

open Printf

type kind = B of int | T of int | V of int | TD of int | END

(*
let compiled_kdtree_file_name = "edgar-compiled.dat"
let compiled_data : float array = input_value (open_in compiled_kdtree_file_name)
*)

let compiled_data : float array = input_value stdin

let data_size = Array.length compiled_data

let todo_list = Hashtbl.create 0
let bb_index  = Hashtbl.create 0
let vtx_index = Hashtbl.create 0
let tri_index = Hashtbl.create 0
let td_index  = Hashtbl.create 0

let bb_next_idx  = ref 0
let tri_next_idx = ref 0
let td_next_idx  = ref 0
let vtx_next_idx = ref 0

let todo_bb i = (
    let idx = !bb_next_idx in
    Hashtbl.add bb_index  i idx;
    Hashtbl.add todo_list i (B(idx));
    bb_next_idx:=idx+1;
    idx
)
let todo_tri i = (
    let idx = !tri_next_idx in
(*    printf "    [T%d at %d]\n" idx i; *)
    Hashtbl.add tri_index i idx;
    Hashtbl.add todo_list i (T(idx));
    tri_next_idx:=idx+1;
    idx
)
let todo_vtx i = (
    let idx = !vtx_next_idx in
    printf "    [V%d at %d]\n" idx i;
    Hashtbl.add vtx_index i idx;
    Hashtbl.add todo_list i (V(idx));
    vtx_next_idx:=idx+1;
    idx
)
let todo_td i = (
    let idx = !td_next_idx in
    Hashtbl.add td_index i idx;
    Hashtbl.add todo_list i (TD(idx));
    td_next_idx:=idx+1;
    idx
)

let rec go i =
    let ofs =
    try
        let kind = Hashtbl.find todo_list i in
	    print_kind kind i
	with Not_found ->
	    print_unknown_data_block i
    in
	if ofs > 0
	then go (i+ofs)
and print_kind k i = match k with
    | B(n)  -> print_bb i n
    | T(n)  -> print_t  i n
    | V(n)  -> print_v  i n
    | TD(n) -> print_td i n
    | END   -> 0
and print_unknown_data_block i =
    printf "UNKNOWN %f\n" compiled_data.(i);
    1
and print_t i n =
    printf "T %d:\n" n;
    let a = (vtx_reg (int_of_float compiled_data.(i))) in
    let b = (vtx_reg (int_of_float compiled_data.(i+1))) in
    let c = (vtx_reg (int_of_float compiled_data.(i+2))) in
    let d = (td_reg  (int_of_float compiled_data.(i+3))) in
    printf "    <V%d, V%d, V%d | TD%d>\n" a b c d;
    4
and print_v i n =
    printf "V %d:\n" n;
    printf "    <%f, %f, %f>\n"
	compiled_data.(i)
	compiled_data.(i+1)
	compiled_data.(i+2);
    let rec loop_vtx_data ofs =
	try
	    ignore(Hashtbl.find todo_list (i+ofs)); ofs
	with
	    Not_found ->
	    	printf "    %f\n" compiled_data.(i+ofs);
		loop_vtx_data (ofs+1)
    in
	loop_vtx_data 3
and print_td i n =
    printf "TD %d:\n" n;
    let rec loop_tridata ofs =
	printf "    %f\n" compiled_data.(i+ofs);
	try
	    ignore(Hashtbl.find todo_list (i+ofs+1)); (ofs+1)
	with
	    Not_found -> loop_tridata (ofs+1)
    in
	loop_tridata 0
and bb_reg bb_ofs =
    try
	Hashtbl.find bb_index bb_ofs
    with Not_found ->
	todo_bb bb_ofs
and tri_reg tri_ofs =
    try
	Hashtbl.find tri_index tri_ofs
    with Not_found ->
	todo_tri tri_ofs
and vtx_reg vtx_ofs =
    try
	Hashtbl.find vtx_index vtx_ofs
    with Not_found ->
	todo_vtx vtx_ofs
and td_reg td_ofs =
    try
	Hashtbl.find td_index td_ofs
    with Not_found ->
	todo_td td_ofs
and print_bb i n =
    printf "BB %d:\n" n;
    printf "    <%f, %f, %f> -> <%f, %f, %f>\n"
           compiled_data.(i)
	   compiled_data.(i+1)
	   compiled_data.(i+2)
	   compiled_data.(i+3)
	   compiled_data.(i+4)
	   compiled_data.(i+5);
    let rec loop_bb_objlist ofs =
	if i+ofs = data_size
	then (printf "    Premature end\n"; ofs)
	else
	let a = int_of_float (compiled_data.(i+ofs)) in
	match a land 3 with
	| 0 -> printf "    B%d\n" (bb_reg (a lsr 2)); loop_bb_objlist (ofs+1)
	| 1 -> printf "    T%d\n" (tri_reg (a lsr 2)); loop_bb_objlist (ofs+1)
	| 3 -> ofs+1 (*3*)
	| _ -> printf "    ??? %d\n" a; ofs+1 (* 2 ? *)
    in loop_bb_objlist 6
;;
ignore (todo_bb 0);
Hashtbl.add todo_list data_size END;
go 0
