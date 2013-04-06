open Math3
open Printf
open Opcodes
open Driver

let vm = Vm.vm_new ();;
let fn = Sys.argv.(1);;

let pc = ref 0;;
let output_file_name = ref None;;
let xsz = ref 256;;
let ysz = ref 256;;
let scene = ref None;;

(* The regular expressions are not Perl-style, ugly *)
let rx_pc = Str.regexp "^pc=\\([0-9]+\\)$";;
let rx_x = Str.regexp "^x=\\([0-9]+\\)$";;
let rx_y = Str.regexp "^y=\\([0-9]+\\)$";;
let rx_scene = Str.regexp "^scene=\\(.+\\)$";;
let rx_fn = Str.regexp "^fn=\\(.+\\)$";;
let rx = Str.regexp "^\\([A-Z0-9]+\\)\\.\\([xyzw]\\)=\\(-?[0-9]+\\.[0-9]+\\)$";;

for i = 2 to (Array.length Sys.argv - 1) do
    let a = Sys.argv.(i) in
    if Str.string_match rx_x a 0
    then (
        xsz := int_of_string (Str.matched_group 1 a)
    )
    else if Str.string_match rx_y a 0
    then (
        ysz := int_of_string (Str.matched_group 1 a)
    )
    else if Str.string_match rx_scene a 0
    then (
        scene := Some (Str.matched_group 1 a)
    )
    else if Str.string_match rx_fn a 0
    then (
        output_file_name := Some (Str.matched_group 1 a)
    )
    else if Str.string_match rx_pc a 0
    then (
        pc := int_of_string (Str.matched_group 1 a)
    )
    else if Str.string_match rx a 0
    then (
        let r = Str.matched_group 1 a in
        let s = Str.matched_group 2 a in
        let v = float_of_string (Str.matched_group 3 a) in
        let rn = (match compile_regname r with
                 | #target_register as reg -> reg
                 |`HIT_TRI -> failwith "FIXME: Setting HIT_TRI not implemented yet"
                 |`HIT_OBJ -> failwith "FIXME: Setting HIT_OBJ not implemented yet"
                 )in
        let sn = (match s with
        | "x" -> 0
        | "y" -> 1
        | "z" -> 2
        | "w" -> 3
        | _   -> failwith "Internal error: Regular expression [xyzw] matched something else than [xyzw]"
        ) in
        Vm.vm_setsubreg vm rn sn v
    ) else (
        failwith (sprintf "Argument cannot be parsed: %s" a)
    )
done;;

let xsz = !xsz;;
let ysz = !ysz;;
let output_file = match !output_file_name with Some fn -> open_out fn | None -> stdout;;


let pixel (r,g,b) =
    fprintf output_file "%c%c%c"
        (pixel_conversion r)
        (pixel_conversion g)
        (pixel_conversion b);;

vm_load_asm vm fn;;
match !scene with Some s -> vm_load_scene vm s | None -> ();;

begin
    fprintf output_file "P6\n%d %d\n255\n" xsz ysz;
    for y = 0 to (ysz-1) do
        let yi = (float_of_int y) /. (float_of_int (ysz-1)) in
        for x = 0 to (xsz-1) do
            let xi = (float_of_int x) /. (float_of_int (xsz-1)) in
            Vm.vm_setreg vm reg_index_R0 xi yi 0.0 0.0;
            vm.Vm.pc <- !pc;
            Vm.vm_run_shader vm;
	    pixel (
                vm.Vm.regs.(reg_index_R0).Vm.x,
                vm.Vm.regs.(reg_index_R0).Vm.y,
                vm.Vm.regs.(reg_index_R0).Vm.z
            )
        done
    done
end
