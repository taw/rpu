open Math3
open Printf
open Opcodes
open Driver

let bg_color = (0.0, 0.0, 0.0)

let render_scene vm orig dir0 right up xsz ysz output_file root_bb_addr =
    let pixel (r,g,b) =
	fprintf output_file "%c%c%c"
	        (pixel_conversion r)
		(pixel_conversion g)
		(pixel_conversion b)
in
    let run_shader hit_point dir tri_data hit_dist =
    (
	Vm.vm_setreg vm reg_index_R0 hit_point.x hit_point.y hit_point.z hit_dist;
	Vm.vm_setreg vm reg_index_R1 dir.x dir.y dir.z 1.0;
	Vm.vm_setreg vm reg_index_A tri_data 0.0 0.0 0.0;
	vm.Vm.pc <- 0;
	Vm.vm_run_shader vm;
	pixel (
		vm.Vm.regs.(reg_index_R0).Vm.x,
		vm.Vm.regs.(reg_index_R0).Vm.y,
		vm.Vm.regs.(reg_index_R0).Vm.z
	    )
    )
in
    let hit_result orig dir = function
	| None   -> pixel bg_color
	| Some (hit_dist, tri_data, _, _) ->
	    run_shader (orig +& (dir *& hit_dist)) dir (float_of_int tri_data) hit_dist
in
    let trace_ray y x =
	let xofs = right *& (-1.0 +. 2.0 *. (float_of_int x) /. (float_of_int (xsz - 1))) in
	let yofs = up    *& ( 1.0 -. 2.0 *. (float_of_int y) /. (float_of_int (ysz - 1))) in
	let dir  = normalize (dir0 +& xofs +& yofs) in
	(dir,
	 Hardware.trace orig dir (0.0, infinity) vm.Vm.data_mem root_bb_addr
	)
in
    begin
     fprintf output_file "P6\n%d %d\n255\n" xsz ysz;
     for y = 0 to (ysz-1) do
	for x = 0 to (xsz-1) do
	    let dir, hit_info = (trace_ray y x)
	    in hit_result orig dir hit_info
	done;
     done
end

(* TESTS *)

(* TEST 2 - constant color triangles *)
(*
 * It's supposed to look like that
 * \GGG
 * B\GG
 * BB\G
 * BBB\
 * BBBB\RRR
 * BBBBB\RR
 * BBBBBB\R
 * BBBBBBB\
 *)

let test_scene_2_data = DynArray.of_list [
(* Root bounding box *)
    -1.0 ; -1.0; 1.5; (* min *)
     1.0;   1.0; 1.5; (* max *)
     41.0; (* obj_0 << 2 + 1 *)
     69.0; (* obj_1 << 2 + 1 *)
     97.0; (* obj_2 << 2 + 1 *)
     3.0;
(* obj_0 : 10 *)
    31.0; 34.0; 37.0; 14.0; (* v0; v1; v2; t0; *)
(* t_0 : 14 *)
     1.0;  0.0; 0.0; (* r g b *)
(* obj_1 : 17 *)
    40.0; 43.0; 34.0; 21.0; (* v3; v4; v1; t1; *)
(* t_1 : 21 *)
     0.0;  1.0; 0.0; (* r g b *)
(* obj_2 : 24 *)
    40.0; 46.0; 37.0; 28.0; (* v3; v5; v2; t2; *)
(* t_2 : 28 *)
     0.0;  0.0; 1.0; (* r g b *)
(* v0 : 31 *)
    -1.0; -1.0; 1.5;
(* v1 : 34 *)
     1.0; -1.0; 1.5;
(* v2 : 37 *)
    -1.0;  1.0; 1.5;
(* v3 : 40 *)
     0.0;  0.0; 1.5;
(* v4 : 43 *)
     1.0;  0.0; 1.5;
(* v5 : 46 *)
     0.0;  1.0; 1.5;
];;

let test_scene_2_code = DynArray.of_list [
(* LOAD I0, A0 *)
    `LOAD(0, 0, 0);
(* MOV A.y, I0.w *)
    `MOV(
	(`Reg(reg_index_A), {wb_x=false; wb_y=true; wb_z=false; wb_w=false}, `NOP),
	(`Reg(`Reg(reg_index_I0), {sw_x=3; sw_y=3; sw_z=3; sw_w=3}, 1.0))
	);
(* LOAD I1, A0 *)
    `LOAD(1, 1, 0);
(* MOV R0.xyz, I0.xwy *)
    `MOV(
	(`Reg(reg_index_R0),      {wb_x=true; wb_y=true; wb_z=true; wb_w=false}, `NOP),
	(`Reg(`Reg(reg_index_I1), {sw_x=1;    sw_y=2;    sw_z=0;    sw_w=0;}, 1.0))
	);
(* RETURN *)
    `RETURN;
];;

let test_2 : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_2_data vm.Vm.data_mem;
    DynArray.append test_scene_2_code vm.Vm.code_mem;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz (open_out "test2.pnm") 0;
)
;;
test_2()
