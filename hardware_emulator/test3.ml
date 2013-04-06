open Math3
open Printf
open Opcodes
open Driver

let bg_color = (0.0, 0.0, 0.0)

let render_scene vm orig dir0 right up xsz ysz output_filename root_bb_addr =
    let output_file = open_out output_filename
in
    let output_header () =
	output_string output_file (sprintf "P6\n%d %d\n255\n" xsz ysz)
in
    let output_pixel (r,g,b) =
    (
	output_char output_file (pixel_conversion r);
	output_char output_file (pixel_conversion g);
	output_char output_file (pixel_conversion b)
    )
in
    let run_shader hit_point dir (tri_data,hit_dist,u,v) =
    (
	Vm.vm_setreg vm reg_index_R0 hit_point.x hit_point.y hit_point.z hit_dist;
	Vm.vm_setreg vm reg_index_R1 dir.x dir.y dir.z 1.0;
	Vm.vm_setreg vm reg_index_R2 (1.0-.u-.v) u v 1.0;
	Vm.vm_setreg vm reg_index_A tri_data 0.0 0.0 0.0;
	vm.Vm.pc <- 0;
	Vm.vm_run_shader vm;
	output_pixel (
		vm.Vm.regs.(reg_index_R0).Vm.x,
		vm.Vm.regs.(reg_index_R0).Vm.y,
		vm.Vm.regs.(reg_index_R0).Vm.z
	    )
    )
in
    let hit_result orig dir = function
	| None   -> output_pixel bg_color
	| Some (hit_dist, tri_data, u, v) ->
	    run_shader (orig +& (dir *& hit_dist)) dir ((float_of_int tri_data), hit_dist, u, v)
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
     output_header ();
     for y = 0 to (ysz-1) do
	for x = 0 to (xsz-1) do
	    let dir, hit_info = (trace_ray y x)
	    in hit_result orig dir hit_info
	done;
     done;
     close_out output_file;
end

(* TESTS *)

(* TEST 3 - texture-related stuff *)

let test_scene_3_data_full = DynArray.of_list [
(* Root bounding box *)
    -1.0 ; -1.0; 1.5; (* min *)
     1.0;   1.0; 1.5; (* max *)
     41.0; (* obj_0 << 2 + 1 *)
     57.0; (* obj_1 << 2 + 1 *)
     73.0; (* obj_2 << 2 + 1 *)
     3.0;
(* obj_0 : 10 *)
    22.0; 27.0; 32.0; 0.0; (* v0; v1; v2; X; *)
(* obj_1 : 14 *)
    37.0; 42.0; 27.0; 0.0; (* v3; v4; v1; X; *)
(* obj_2 : 18 *)
    37.0; 47.0; 32.0; 0.0; (* v3; v5; v2; X; *)
(* v0 : 22 *)
    -1.0; -1.0; 1.5; (* x y z *)
     0.0;  1.0;      (* s t *)
(* v1 : 27 *)
     1.0; -1.0; 1.5; (* x y z *)
     1.0;  1.0;      (* s t *)
(* v2 : 32 *)
    -1.0;  1.0; 1.5; (* x y z *)
     0.0;  0.0;      (* s t *)
(* v3 : 37 *)
     0.0;  0.0; 1.5; (* x y z *)
     0.5;  0.5;      (* s t *)
(* v4 : 42 *)
     1.0;  0.0; 1.5; (* x y z *)
     1.0;  0.5;      (* s t *)
(* v5 : 47 *)
     0.0;  1.0; 1.5; (* x y z *)
     0.5;  0.0;      (* s t *)
];;

let test_scene_3_data_closeup = DynArray.of_list [
(* Root bounding box *)
    -1.0 ; -1.0; 1.5; (* min *)
     1.0;   1.0; 1.5; (* max *)
     41.0; (* obj_0 << 2 + 1 *)
     57.0; (* obj_1 << 2 + 1 *)
     73.0; (* obj_2 << 2 + 1 *)
     3.0;
(* obj_0 : 10 *)
    22.0; 27.0; 32.0; 0.0; (* v0; v1; v2; X; *)
(* obj_1 : 14 *)
    37.0; 42.0; 27.0; 0.0; (* v3; v4; v1; X; *)
(* obj_2 : 18 *)
    37.0; 47.0; 32.0; 0.0; (* v3; v5; v2; X; *)
(* v0 : 22 *)
    -1.0; -1.0; 1.5; (* x y z *)
     0.0;  0.2;      (* s t *)
(* v1 : 27 *)
     1.0; -1.0; 1.5; (* x y z *)
     0.2;  0.2;      (* s t *)
(* v2 : 32 *)
    -1.0;  1.0; 1.5; (* x y z *)
     0.0;  0.0;      (* s t *)
(* v3 : 37 *)
     0.0;  0.0; 1.5; (* x y z *)
     0.1;  0.1;      (* s t *)
(* v4 : 42 *)
     1.0;  0.0; 1.5; (* x y z *)
     0.2;  0.1;      (* s t *)
(* v5 : 47 *)
     0.0;  1.0; 1.5; (* x y z *)
     0.1;  0.0;      (* s t *)
];;

let test_scene_3_code_uv = DynArray.of_list [
(* LOAD I0, A0 *)
    `LOAD(0, 0, 0); (* A_data B_data C_data X *)
(* MOV A.yzw I0.xxyz *)
    `MOV(
	(`Reg(reg_index_A), {wb_x=false; wb_y=true; wb_z=true; wb_w=true;}, `NOP),
	(`Reg(`Reg(reg_index_I0), {sw_x=0; sw_y=0; sw_z=1; sw_w=2;}, 1.0))
	);
(* LOAD I1, A1 *)
    `LOAD(1, 1, 1); (* X X As At *)
(* LOAD I2, A2 *)
    `LOAD(2, 2, 1); (* X X Bs Bt *)
(* LOAD I3, A3 *)
    `LOAD(3, 3, 1); (* X X Cs Ct *)

(* s/t computation *)
(* MUL R34.xy, I1.zw, R2.xx        : Point(s/t)  = A(s/t) * (1-u-v) *)
    `MUL(
	(`Reg(reg_index_R3), {wb_x=true; wb_y=true; wb_z=false; wb_w=false}, `NOP),
	(`Reg(`Reg(reg_index_I1), {sw_x=2; sw_y=3; sw_z=3; sw_w=3;}, 1.0)),
	(`Reg(`Reg(reg_index_R2), {sw_x=0; sw_y=0; sw_z=0; sw_w=0;}, 1.0))
	);
(* MAD R3.xy, I2.zw, R2.yy, R3.xy : Point(s/t) += B(s/t) * u *)
    `MAD(
	(`Reg(reg_index_R3), {wb_x=true; wb_y=true; wb_z=false; wb_w=false}, `NOP),
	(`Reg(`Reg(reg_index_I2), {sw_x=2; sw_y=3; sw_z=3; sw_w=3;}, 1.0)),
	(`Reg(`Reg(reg_index_R2), {sw_x=1; sw_y=1; sw_z=1; sw_w=1;}, 1.0)),
	(`Reg(`Reg(reg_index_R3), {sw_x=0; sw_y=1; sw_z=1; sw_w=1;}, 1.0))
	);
(* MAD R3.xy, I3.zw, R2.zz, R3.xy : Point(s/t) += C(s/t) * v *)
    `MAD(
	(`Reg(reg_index_R3), {wb_x=true; wb_y=true; wb_z=false; wb_w=false}, `NOP),
	(`Reg(`Reg(reg_index_I3), {sw_x=2; sw_y=3; sw_z=3; sw_w=3;}, 1.0)),
	(`Reg(`Reg(reg_index_R2), {sw_x=2; sw_y=2; sw_z=2; sw_w=2;}, 1.0)),
	(`Reg(`Reg(reg_index_R3), {sw_x=0; sw_y=1; sw_z=1; sw_w=1;}, 1.0))
	);
(* MOV R0.xy, R3.xy  = s/t *)
    `MOV(
	(`Reg(reg_index_R0), {wb_x=true; wb_y=true; wb_z=false; wb_w=false}, `NOP),
	(`Reg(`Reg(reg_index_R3), swizzle_none, 1.0))
	);
(* MOV R0.z, 1.0 # for better visibility near s/t=0/0 *)
    `MOV(
	(`Reg(reg_index_R0), {wb_x=false; wb_y=false; wb_z=true; wb_w=false}, `NOP),
	(`Imm(1.0))
	);
(* RETURN *)
    `RETURN;
]

let test_scene_3_code_texture = DynArray.of_list [
(* LOAD I0, A0 *)
    `LOAD(0, 0, 0); (* A_data B_data C_data X *)
(* MOV A.yzw I0.xxyz *)
    `MOV(
	(`Reg(reg_index_A), {wb_x=false; wb_y=true; wb_z=true; wb_w=true;}, `NOP),
	(`Reg(`Reg(reg_index_I0), {sw_x=0; sw_y=0; sw_z=1; sw_w=2;}, 1.0))
	);
(* LOAD I1, A1 *)
    `LOAD(1, 1, 1); (* X X As At *)
(* LOAD I2, A2 *)
    `LOAD(2, 2, 1); (* X X Bs Bt *)
(* LOAD I3, A3 *)
    `LOAD(3, 3, 1); (* X X Cs Ct *)

(* s/t computation *)
(* MUL R34.xy, I1.zw, R2.xx        : Point(s/t)  = A(s/t) * (1-u-v) *)
    `MUL(
	(`Reg(reg_index_R3), {wb_x=true; wb_y=true; wb_z=false; wb_w=false}, `NOP),
	(`Reg(`Reg(reg_index_I1), {sw_x=2; sw_y=3; sw_z=3; sw_w=3;}, 1.0)),
	(`Reg(`Reg(reg_index_R2), {sw_x=0; sw_y=0; sw_z=0; sw_w=0;}, 1.0))
	);
(* MAD R3.xy, I2.zw, R2.yy, R3.xy : Point(s/t) += B(s/t) * u *)
    `MAD(
	(`Reg(reg_index_R3), {wb_x=true; wb_y=true; wb_z=false; wb_w=false}, `NOP),
	(`Reg(`Reg(reg_index_I2), {sw_x=2; sw_y=3; sw_z=3; sw_w=3;}, 1.0)),
	(`Reg(`Reg(reg_index_R2), {sw_x=1; sw_y=1; sw_z=1; sw_w=1;}, 1.0)),
	(`Reg(`Reg(reg_index_R3), {sw_x=0; sw_y=1; sw_z=1; sw_w=1;}, 1.0))
	);
(* MAD R3.xy, I3.zw, R2.zz, R3.xy : Point(s/t) += C(s/t) * v *)
    `MAD(
	(`Reg(reg_index_R3), {wb_x=true; wb_y=true; wb_z=false; wb_w=false}, `NOP),
	(`Reg(`Reg(reg_index_I3), {sw_x=2; sw_y=3; sw_z=3; sw_w=3;}, 1.0)),
	(`Reg(`Reg(reg_index_R2), {sw_x=2; sw_y=2; sw_z=2; sw_w=2;}, 1.0)),
	(`Reg(`Reg(reg_index_R3), {sw_x=0; sw_y=1; sw_z=1; sw_w=1;}, 1.0))
	);
(* MOV R4, 0.0 : Texture 0 *) (* FIXME: Some saner texture iface *)
    `MOV(
	(`Reg(reg_index_R4), wbm_all, `NOP),
	(`Imm(0.0))
	);
(* TEXLOAD I2, R4, R3 *)
    `TEXLOAD(
	2,
	(`Reg(`Reg(reg_index_R4), swizzle_none, 1.0)),
	(`Reg(`Reg(reg_index_R3), swizzle_none, 1.0))
    );
(* MOV R0.xyz, I2 *)
    `MOV(
	(`Reg(reg_index_R0), {wb_x=true; wb_y=true; wb_z=true; wb_w=false}, `NOP),
	(`Reg(`Reg(reg_index_I2), swizzle_none, 1.0))
	);
(* RETURN *)
    `RETURN;
];;

let test_3_texture_1_data_flt = Array.create (320*320*3) 0.0
let test_3_texture_1_data_flt4= Array.create (320*320*4) 0.0
let test_3_texture_1_data_byte= Array.create (320*320*3) '\000'

let load_texture_file =
    let fh = open_in "texture1.rgb" in
    for i = 0 to 320*320-1 do
	let r = input_char fh in
	let g = input_char fh in
	let b = input_char fh in

	test_3_texture_1_data_byte.(3*i)   <- r;
	test_3_texture_1_data_byte.(3*i+1) <- g;
	test_3_texture_1_data_byte.(3*i+2) <- b;

	test_3_texture_1_data_flt.(3*i)   <- (float_of_int (Char.code r)) /. 255.0;
	test_3_texture_1_data_flt.(3*i+1) <- (float_of_int (Char.code g)) /. 255.0;
	test_3_texture_1_data_flt.(3*i+2) <- (float_of_int (Char.code b)) /. 255.0;

	test_3_texture_1_data_flt4.(4*i)   <- (float_of_int (Char.code r)) /. 255.0;
	test_3_texture_1_data_flt4.(4*i+1) <- (float_of_int (Char.code g)) /. 255.0;
	test_3_texture_1_data_flt4.(4*i+2) <- (float_of_int (Char.code b)) /. 255.0;
	test_3_texture_1_data_flt4.(4*i+3) <- 1.0;
    done

(* Prepare texture descriptors *)

let test_3_texture_1_bilinear_flt =
    make_texture_descriptor (`LINEAR,`LINEAR) (`CLAMP,`CLAMP) (320,320) (`FLT3(test_3_texture_1_data_flt))

let test_3_texture_1_nn_flt =
    make_texture_descriptor (`NEAREST,`NEAREST) (`CLAMP,`CLAMP) (320,320) (`FLT3(test_3_texture_1_data_flt))

let test_3_texture_1_nl_flt =
    make_texture_descriptor (`NEAREST,`LINEAR) (`CLAMP,`CLAMP) (320,320) (`FLT3(test_3_texture_1_data_flt))

let test_3_texture_1_ln_flt =
    make_texture_descriptor (`LINEAR,`NEAREST) (`CLAMP,`CLAMP) (320,320) (`FLT3(test_3_texture_1_data_flt))

let test_3_texture_1_bilinear_byte =
    make_texture_descriptor (`LINEAR,`LINEAR) (`CLAMP,`CLAMP) (320,320) (`BYTE3(test_3_texture_1_data_byte))

let test_3_texture_1_nn_byte =
    make_texture_descriptor (`NEAREST,`NEAREST) (`CLAMP,`CLAMP) (320,320) (`BYTE3(test_3_texture_1_data_byte))

let test_3_texture_1_nl_byte =
    make_texture_descriptor (`NEAREST,`LINEAR) (`CLAMP,`CLAMP) (320,320) (`BYTE3(test_3_texture_1_data_byte))

let test_3_texture_1_ln_byte =
    make_texture_descriptor (`LINEAR,`NEAREST) (`CLAMP,`CLAMP) (320,320) (`BYTE3(test_3_texture_1_data_byte))

let test_3_texture_1_bilinear_flt4 =
    make_texture_descriptor (`LINEAR,`LINEAR) (`CLAMP,`CLAMP) (320,320) (`FLT4(test_3_texture_1_data_flt4))

let test_3_texture_1_nn_flt4 =
    make_texture_descriptor (`NEAREST,`NEAREST) (`CLAMP,`CLAMP) (320,320) (`FLT4(test_3_texture_1_data_flt4))

let test_3_texture_1_nl_flt4 =
    make_texture_descriptor (`NEAREST,`LINEAR) (`CLAMP,`CLAMP) (320,320) (`FLT4(test_3_texture_1_data_flt4))

let test_3_texture_1_ln_flt4 =
    make_texture_descriptor (`LINEAR,`NEAREST) (`CLAMP,`CLAMP) (320,320) (`FLT4(test_3_texture_1_data_flt4))

let test_3a : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_full vm.Vm.data_mem;
    DynArray.append test_scene_3_code_uv vm.Vm.code_mem;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_uv.pnm" 0;
);;

let test_3b : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_full vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_bilinear_flt;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_full.pnm" 0;
);;

let test_3c : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_bilinear_flt;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_bilinear.pnm" 0;
);;

let test_3d : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_nn_flt;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_nn.pnm" 0;
);;

let test_3e : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_nl_flt;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_nl.pnm" 0;
);;

let test_3f : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_ln_flt;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_ln.pnm" 0;
);;

let test_3g : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_bilinear_byte;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_bilinear_b.pnm" 0;
);;

let test_3h : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_nn_byte;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_nn_b.pnm" 0;
);;

let test_3i : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_nl_byte;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_nl_b.pnm" 0;
);;

let test_3j : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_ln_byte;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_ln_b.pnm" 0;
);;

let test_3k : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_bilinear_flt4;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_bilinear_f4.pnm" 0;
);;

let test_3l : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_nn_flt4;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_nn_f4.pnm" 0;
);;

let test_3m : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_nl_flt4;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_nl_f4.pnm" 0;
);;

let test_3n : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    DynArray.append test_scene_3_data_closeup vm.Vm.data_mem;
    DynArray.append test_scene_3_code_texture vm.Vm.code_mem;
    DynArray.add vm.Vm.textures test_3_texture_1_ln_flt4;
    let orig  = v3_make 0.0 0.0 0.0 in
    let dir0  = v3_make 0.0 0.0 1.0 in
    let right = v3_make 1.0 0.0 0.0 in
    let up    = v3_make 0.0 1.0 0.0 in
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm orig dir0 right up xsz ysz "test3_closeup_ln_f4.pnm" 0;
);;

test_3a();;
test_3b();;
test_3c();;
test_3d();;
test_3e();;
test_3f();;
test_3g();;
test_3h();;
test_3i();;
test_3j();;
test_3k();;
test_3l();;
test_3m();;
test_3n();;
