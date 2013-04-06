open Printf
open Shader

(*********************************************************************
 * Sample shaders for testing                                        *
 *********************************************************************)

let x = 0
let y = 1
let z = 2
let w = 3

(*
 * Some shortcuts for mask
 *)
let true3=(true,true,true)
let true4=(true,true,true,true)
let no_swizzle=(0,1,2,3) (* (X,Y,Z,W) *)
let no_mod=1.0 (* (false,SM_1) *)
let reg_source n = SrcReg(n,no_swizzle,no_mod)
let return_unconditional = Some(RETURN(true3,true4,RED_OR))


(*
 * mov R15, R7
 * + return or xyzw (>=0 or <1)
 *)
let (shader_const : shader) = [|
  (
    (MOV,None,(15,true4),[reg_source 7]),
     return_unconditional
  )
|]
(* Green at R0.z = 0.8 x=1.0
 * Blue  at R0.z = 1.8 x=0.0
 *
 * R15 = [0, 1-x, x, 1]
 *
 * add_sat R15.y, 1.8,  -R0.z
 * add_sat R15.z, 1, -R15.y
 * + return or xyzw (>=0 or <1)
 *)
let (shader_depth : shader) = [|
  (
    (ADD,Some SAT,(15,(false,true,false,false)),[SrcImm 1.8; SrcReg(0,(z,z,z,z),-1.0)]),
    None
  );
  (
    (ADD,Some SAT,(15,(true,false,true,false)),[SrcImm 1.0; SrcReg(15,(y,y,y,y),-1.0)]),
    return_unconditional
  )
|];;

output_value (open_out "const.shr") shader_const;;
output_value (open_out "depth.shr") shader_depth;;
