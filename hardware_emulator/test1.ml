open Math3
open Printf
open Driver

let bg_color = (0.0, 0.0, 0.0)

let render_scene vm orig dir0 right up xsz ysz output_file root_bb_addr =
    let pixel (r,g,b) =
	fprintf output_file "%c%c%c"
	        (pixel_conversion r)
		(pixel_conversion g)
		(pixel_conversion b)
in
    let hit_result orig dir = function
	| None   -> pixel bg_color
	| Some (dir, triaddr, _, _) -> pixel (
		let tri_data = int_of_float(Memory.fetch_float vm.Vm.data_mem (triaddr + 3)) in
		Memory.fetch_float vm.Vm.data_mem (tri_data + 0),
		Memory.fetch_float vm.Vm.data_mem (tri_data + 1),
		Memory.fetch_float vm.Vm.data_mem (tri_data + 2)
	    )
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
	    let dir, hit = (trace_ray y x)
	    in hit_result orig dir hit
	done;
     done
end

(* TESTS *)

(* TEST 1 - just output a hit/miss map *)
(*
 * It's supposed to look like that
 * \BBB
 * R\BB
 * RR\B
 * RRR\
 * RRRR\GGG
 * RRRRR\GG
 * RRRRRR\G
 * RRRRRRR\
 *)

let test_scene_1_data = DynArray.of_list [
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

let test_1 : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_1_data vm.Vm.data_mem;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz (open_out "test1.pnm") 0;
)
;;
test_1()
