open Printf
open Opcodes
open Driver

let vm = Vm.vm_new ();;
let fn = Sys.argv.(1);;

vm.Vm.pc <- 0;;

(* The regular expressions are not Perl-style, ugly *)
let rx_pc = Str.regexp "^pc=\\([0-9]+\\)$";;
let rx = Str.regexp "^\\([A-Z0-9]+\\)\\.\\([xyzw]\\)=\\(-?[0-9]+\\.[0-9]+\\)$";;

for i = 2 to (Array.length Sys.argv - 1) do
    let a = Sys.argv.(i) in
    if Str.string_match rx_pc a 0
    then (
        let v = int_of_string (Str.matched_group 1 a) in
        vm.Vm.pc <- v
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

vm_load_asm vm fn;;
Vm.vm_run_shader vm;;
Vm.vm_print_regs vm;;
