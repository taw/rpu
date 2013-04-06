open Vm
(*
 * FIXME: loading more than one shader or more than one object is broken
 *)

let vm_load_asm vm file_name =
    let loaded_code = DynArray.of_array (input_value (open_in file_name)) in
    DynArray.append loaded_code vm.code_mem

let vm_load_scene vm file_name =
    let loaded_scene_data = DynArray.of_array (input_value (open_in file_name)) in
    DynArray.append loaded_scene_data vm.data_mem

(*
 * Prepare standard texture descriptor
 * Descriptors not indexed 0..1 x 0..1 must be prepared in a special way
 *)
let make_texture_descriptor
    (intx,inty)
    (modx,mody)
    (xsz,ysz)
    data
=
{
    tx_data = data;
    tx_xsz  = xsz;
    tx_ysz  = ysz;
    tx_modx = modx;
    tx_mody = mody;
    tx_intx = intx;
    tx_inty = inty;
    tx_xmul = float_of_int(xsz-1);
    tx_ymul = float_of_int(ysz-1);
}

(* Clamp and convert from (0.0,1.0) float to (0,255) byte *)
let pixel_conversion x =
    let a = int_of_float (0.5 +. 255.0 *. x)
 in if a < 0
    then Char.chr 0
    else if a > 255
    then Char.chr 255
    else Char.chr a
