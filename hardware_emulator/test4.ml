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
    let run_shader hit_point dir (tri_data,hit_dist,u,v) =
    (
	Vm.vm_setreg vm reg_index_R0 hit_point.x hit_point.y hit_point.z hit_dist;
	Vm.vm_setreg vm reg_index_R1 dir.x dir.y dir.z 1.0;
	Vm.vm_setreg vm reg_index_R2 (1.0-.u-.v) u v 1.0;
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
	| Some (hit_dist, tri_data, u, v) ->
	    run_shader (orig +& (dir *& hit_dist)) dir ((float_of_int tri_data),hit_dist,u,v)
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

(*
 * <0.8     - green [0, 1, 0]
 * 0.8..1.8 - from green to pink
 * >1.8     - pink [1, 0, 1]
 *)
let test_scene_4_code = DynArray.of_list [
(* ADD_SAT R0.y, 1.8, -R0.w *)
    `ADD(
	(`Reg(reg_index_R0), {wb_x=false; wb_y=true; wb_z=false; wb_w=false}, `SAT),
	(`Imm(1.8)),
	(`Reg(`Reg(reg_index_R0), {sw_x=3; sw_y=3; sw_z=3; sw_w=3;}, -1.0))
	);
(* ADD_SAT R0.xz, 1.0, -R0.y *)
    `ADD(
	(`Reg(reg_index_R0), {wb_x=true; wb_y=false; wb_z=true; wb_w=false}, `SAT),
	(`Imm(1.0)),
	(`Reg(`Reg(reg_index_R0), {sw_x=1; sw_y=1; sw_z=1; sw_w=1}, -1.0))
	);
(* RETURN *)
    `RETURN;
];;

let test_4 : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    vm_load_scene vm "bunny-compiled.dat";
    DynArray.append test_scene_4_code vm.Vm.code_mem;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz (open_out "test4.pnm") 0;
)
;;
test_4()
