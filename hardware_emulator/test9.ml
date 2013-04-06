open Printf
open Opcodes
open Driver

let render_scene vm xsz ysz output_file root_bb_addr =
    let pixel (r,g,b) =
	fprintf output_file "%c%c%c"
	        (pixel_conversion r)
		(pixel_conversion g)
		(pixel_conversion b)
in
    let run_shader x y =
    (
	Vm.vm_setreg vm reg_index_C0 0.0 (0.20) (-1.0) 0.0;    (* orig *)
	Vm.vm_setreg vm reg_index_C1 0.0 0.0 (-1.0) 0.00;      (* dir.x = C1 . P *)
	Vm.vm_setreg vm reg_index_C2 0.0 (-1.0) 0.5 0.00;      (* dir.y = C2 . P *)
	Vm.vm_setreg vm reg_index_C3 1.0 0.0 (-0.5) 0.00;      (* dir.z = C3 . P *)
	Vm.vm_setreg vm reg_index_C4 0.0 (infinity) 0.0 0.0;   (* trace spec *)
	Vm.vm_setreg vm reg_index_C5 (-0.2) 0.5 0.0 1.0;       (* light source *)
	Vm.vm_setreg vm reg_index_C6 0.2 0.2 1.0 0.0;          (* bg color *)
	Vm.vm_setreg vm reg_index_P  x y 0.0 0.0;
	vm.Vm.pc <- 0;
	Vm.vm_run_shader vm;
	pixel (
		vm.Vm.regs.(reg_index_R0).Vm.x,
		vm.Vm.regs.(reg_index_R0).Vm.y,
		vm.Vm.regs.(reg_index_R0).Vm.z
	    )
    )
in
    begin
     fprintf output_file "P6\n%d %d\n255\n" xsz ysz;
     for y = 0 to (ysz-1) do
	for x = 0 to (xsz-1) do
	    run_shader (float_of_int x /. float_of_int (xsz-1)) (float_of_int y /. float_of_int (ysz-1))
	done
     done
end

(* TESTS *)

let test_9 : unit -> unit = fun () ->
(
    let vm = Vm.vm_new () in
    vm_load_scene vm "edgar-compiled.dat";
    vm_load_asm vm "phong_normal.shr";
    let xsz   = 400 in
    let ysz   = 400 in
    render_scene vm xsz ysz (open_out "test9.pnm") 0;
)
;;
test_9()
