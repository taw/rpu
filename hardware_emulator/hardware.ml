open Math3
open Intersection_tests
open Printf

(*
 * Memory layout
 * Triangle:
 * [vertex A ptr, vertex B ptr, vertex C ptr, triangle data ptr] : (as words)
 *
 * vertex data:
 * [x, y, z, anything]
 *
 * triangle data:
 * [anything]
 *
 * BB:
 * [x0, y0, z0, x1, y1, z1, obj0, obj1, obj2, ...]
 * obj (addr shifted 2 bits left, and the last 2 bits serve as a flag):
 * x 00 - bb  (addr<<2) + 00
 * x 01 - tri (addr<<2) + 01
 * ? 11 - end
 *
 * TODO: Add some accelerator structure ?
 *
 * returns Some (hit_dist, triangle_addr) or None
 * t0 <= hit_dist <= t1
 *)

let trace orig dir (t0, t1) mem root_bb_addr =
    let cur_t1 = ref t1 in (* either best hit or search limit *)
    let hit    = ref (-1, 0.0, 0.0) in
    let idir = componentwise_inverse dir in
    let hit_triangle addr =
	let a_addr = int_of_float (Memory.fetch_float mem addr) in
	let a      = Memory.fetch_v3 mem a_addr in
	let b_addr = int_of_float (Memory.fetch_float mem (addr+1)) in
	let b      = Memory.fetch_v3 mem b_addr in
	let c_addr = int_of_float (Memory.fetch_float mem (addr+2)) in
	let c      = Memory.fetch_v3 mem c_addr in
	match ray_triangle_intersection_test orig dir (a,b,c) with
	| Some (hit_dist, u, v) ->
	    if hit_dist < !cur_t1
	    then
		(
		cur_t1 := hit_dist;
		hit    := (addr, u, v);
		)
	| None -> ()
    in
    let rec traverse_bb_tree bb_addr =
	let bb_min  = Memory.fetch_v3 mem (bb_addr) in
	let bb_max  = Memory.fetch_v3 mem (bb_addr+3) in
	if ray_bb_intersection_test orig idir bb_min bb_max (t0, !cur_t1)
	then (* HIT *)
	    traverse_obj_list (bb_addr+6)
	else (* MISS *)
	    ()
    and traverse_obj_list addr =
	let cell_val = int_of_float (Memory.fetch_float mem addr) in
	let flg = cell_val land 3 in
	if flg = 0 (* Nested BB *)
	then
	    (
	    let obj_addr = cell_val lsr 2 in 
	    traverse_bb_tree obj_addr;
	    traverse_obj_list (addr+1)
	    )
	else if flg = 1  (* Triangle *)
	then
	    (
	    let obj_addr = cell_val lsr 2 in 
	    hit_triangle obj_addr;
	    traverse_obj_list (addr+1);
	    )
	else (* End of traversal *)
	    ()
    in
    begin
	traverse_bb_tree root_bb_addr;
	let (tri_data, u, v) = !hit in
	if tri_data = -1
	then
	    None 
	else
	    Some (!cur_t1, tri_data, u, v)
    end
