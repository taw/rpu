open Printf
(*
 * Used by vm and asm, later also by the compiler
 *)

(* Register offsets in the register file *)
let reg_index_Rx      =  0
let reg_index_R0      =  0
let reg_index_R1      =  1
let reg_index_R2      =  2
let reg_index_R3      =  3
let reg_index_R4      =  4
let reg_index_R5      =  5
let reg_index_R6      =  6
let reg_index_R7      =  7
let reg_index_R8      =  8
let reg_index_R9      =  9
let reg_index_R10     = 10
let reg_index_R11     = 11
let reg_index_R12     = 12
let reg_index_R13     = 13
let reg_index_R14     = 14
let reg_index_R15     = 15
let reg_index_Cx      = 16
let reg_index_C0      = 16
let reg_index_C1      = 17
let reg_index_C2      = 18
let reg_index_C3      = 19
let reg_index_C4      = 20
let reg_index_C5      = 21
let reg_index_C6      = 22
let reg_index_C7      = 23
let reg_index_C8      = 24
let reg_index_C9      = 25
let reg_index_C10     = 26
let reg_index_C11     = 27
let reg_index_C12     = 28
let reg_index_C13     = 29
let reg_index_C14     = 30
let reg_index_C15     = 31
let reg_index_C16     = 32
let reg_index_C17     = 33
let reg_index_C18     = 34
let reg_index_C19     = 35
let reg_index_C20     = 36
let reg_index_C21     = 37
let reg_index_C22     = 38
let reg_index_C23     = 39
let reg_index_C24     = 40
let reg_index_C25     = 41
let reg_index_C26     = 42
let reg_index_C27     = 43
let reg_index_C28     = 44
let reg_index_C29     = 45
let reg_index_C30     = 46
let reg_index_C31     = 47
let reg_index_Ix      = 48
let reg_index_I0      = 48
let reg_index_I1      = 49
let reg_index_I2      = 50
let reg_index_I3      = 51
let reg_index_S       = 52
let reg_index_P       = 53
let reg_index_HIT     = 54
let reg_index_A       = 55

type swizzle = { sw_x : int; sw_y : int; sw_z : int; sw_w : int } (* 0..3 *)
let swizzle_none = {sw_x=0; sw_y=1; sw_z=2; sw_w=3}

type smult   = float (* +/- 0.5 1.0 2.0 4.0 only *)
(* TODO: There are some limitations on wha can be read/written to,
 * different number of ports/register etc. *)
type target_register =
[
| `Reg   of int (* 0..size of register file *)
| `Stack of int (* 0..7, top of the stack *)
]
type source_register =
[
| `Reg   of int (* 0..size of register file *)
| `Stack of int (* 0..7, top of the stack *)
| `HIT_TRI
| `HIT_OBJ
]
type imod = [
| `SAT
| `RCP of int (* 0..3 *)
| `RSQ of int (* 0..3 *)
| `NOP
]
type writeback_mask =  { wb_x : bool; wb_y : bool; wb_z : bool; wb_w : bool }
let wbm_all = { wb_x=true; wb_y=true; wb_z=true; wb_w=true }

type cond = { c_and : bool;
              c_lt0 : bool;
              c_eq0 : bool;
	      c_b01 : bool;
	      c_ge1 : bool;
	      c_x   : bool;
	      c_y   : bool;
	      c_z   : bool;
	      c_w   : bool;
	    }

type source = [
|`Imm of float
|`Reg of source_register * swizzle * smult
]
type writeback = target_register * writeback_mask * imod

type addr_register = int (* 0..3 *)
type input_register = int (* 0..3 *)
type addr_shift = int (* keep small *)
type texture_descriptor = source (* texture_id, X, X, X *)

(* FIXME: full set of ops *)
type op_arith = [
|`MOV  of writeback * source
|`FRAC of writeback * source
|`ADD  of writeback * source * source
|`MUL  of writeback * source * source
|`MAD  of writeback * source * source * source
|`DP3  of writeback * source * source
|`DP4  of writeback * source * source
|`DP2H of writeback * source * source
|`DP3H of writeback * source * source
]
type op_conditional = [
|`JMP of int
|`RETURN
|`CALL of int * int (* call_addr, push_size *)
|`DCALL of int
]
type op = [
| op_arith
| op_conditional (* don't check the condition, just do it *)
|`COND of (op_arith * cond * op_conditional)
|`STORE of addr_register * addr_shift * source
|`TEXLOAD of input_register * texture_descriptor * source
|`TEXLOAD4 of texture_descriptor * source
(* also secondary ? *)
|`RETURN
|`TRACE of source * source * source (* orig, dir, (t0, t1, _, _) *)
|`LOAD  of input_register * addr_register * addr_shift
|`LOAD4 of addr_register * addr_shift
]

(*****************************************************************
 * Printing functions, used to print both Asm_ast and Opcode.op  *
 *****************************************************************
 *)

let sw_to_string sw =
    let sw1 = function
    | 0 -> "x"
    | 1 -> "y"
    | 2 -> "z"
    | 3 -> "w"
    | _ -> failwith "Internal error: swizzle index not in 0..3"
 in if sw = {sw_x=0; sw_y=1; sw_z=2; sw_w=3}
    then ""
    else "."^(sw1 sw.sw_x)^
             (sw1 sw.sw_y)^
	     (sw1 sw.sw_z)^
	     (sw1 sw.sw_w)

let sm_to_string : float -> string = function
| 0.5 -> "0.5*"
| 1.0 -> ""
| 2.0 -> "2.0*"
| 4.0 -> "4.0*"
|(-0.5) -> "-0.5*"
|(-1.0) -> "-"
|(-2.0) -> "-2.0*"
|(-4.0) -> "-4.0*"
| x   -> failwith (sprintf "Internal error: Illegal source multiplier %f" x)

let wbm_to_string wbm =
    if (wbm.wb_x && wbm.wb_y && wbm.wb_z && wbm.wb_w)
    then ""
    else
    "."^
    (if wbm.wb_x then "x" else "")^
    (if wbm.wb_y then "y" else "")^
    (if wbm.wb_z then "z" else "")^
    (if wbm.wb_w then "w" else "")

let cond_to_string c = 
    (if c.c_and then "and" else "or")^
    " "^
    (if c.c_x then "x" else "")^
    (if c.c_y then "y" else "")^
    (if c.c_z then "z" else "")^
    (if c.c_w then "w" else "")^
    (if c.c_x||c.c_y||c.c_z||c.c_w then "" else failwith "Data error: at least one of the mask elements should be true")^
    " "^
    (match (c.c_lt0, c.c_eq0, c.c_b01, c.c_ge1) with
    | ( true, true, true, true) -> "(>=0 or <1)" (* always *)
    | ( true, true, true,false) -> "<1"
    | ( true, true,false, true) -> "(<=0 or >=1)"
    | ( true, true,false,false) -> "<=0"
    | (false, true, true, true) -> ">=0"
    | (false, true, true,false) -> "(>=0 and <1)"
    | (false, true,false, true) -> "(=0 or >=1)"
    | (false, true,false,false) -> "=0"
    | ( true,false, true, true) -> "(!=0)"
    | ( true,false, true,false) -> "(!=0 and <1)"
    | ( true,false,false, true) -> "(<0 or >=1)"
    | ( true,false,false,false) -> "<0"
    | (false,false, true, true) -> ">0"
    | (false,false, true,false) -> "(>0 and <1)"
    | (false,false,false, true) -> ">=1"
    | (false,false,false,false) -> "(<0 and >=1)" (* never *)
    )

let register_to_string : int -> string
= function
|  0 -> "R0"
|  1 -> "R1"
|  2 -> "R2"
|  3 -> "R3"
|  4 -> "R4"
|  5 -> "R5"
|  6 -> "R6"
|  7 -> "R7"
|  8 -> "R8"
|  9 -> "R9"
| 10 -> "R10"
| 11 -> "R11"
| 12 -> "R12"
| 13 -> "R13"
| 14 -> "R14"
| 15 -> "R15"
| 16 -> "C0"
| 17 -> "C1"
| 18 -> "C2"
| 19 -> "C3"
| 20 -> "C4"
| 21 -> "C5"
| 22 -> "C6"
| 23 -> "C7"
| 24 -> "C8"
| 25 -> "C9"
| 26 -> "C10"
| 27 -> "C11"
| 28 -> "C12"
| 29 -> "C13"
| 30 -> "C14"
| 31 -> "C15"
| 32 -> "C16"
| 33 -> "C17"
| 34 -> "C18"
| 35 -> "C19"
| 36 -> "C20"
| 37 -> "C21"
| 38 -> "C22"
| 39 -> "C23"
| 40 -> "C24"
| 41 -> "C25"
| 42 -> "C26"
| 43 -> "C27"
| 44 -> "C28"
| 45 -> "C29"
| 46 -> "C30"
| 47 -> "C31"
| 48 -> "I0"
| 49 -> "I1"
| 50 -> "I2"
| 51 -> "I3"
| 52 -> "S"
| 53 -> "P"
| 54 -> "HIT"
| 55 -> "A"
| r  -> failwith (sprintf "Internal error: register number %d out of range 0..23" r)

let compile_regname = function
| "R0"  -> `Reg(0)
| "R1"  -> `Reg(1)
| "R2"  -> `Reg(2)
| "R3"  -> `Reg(3)
| "R4"  -> `Reg(4)
| "R5"  -> `Reg(5)
| "R6"  -> `Reg(6)
| "R7"  -> `Reg(7)
| "R8"  -> `Reg(8)
| "R9"  -> `Reg(9)
| "R10" -> `Reg(10)
| "R11" -> `Reg(11)
| "R12" -> `Reg(12)
| "R13" -> `Reg(13)
| "R14" -> `Reg(14)
| "R15" -> `Reg(15)
| "C0"  -> `Reg(16)
| "C1"  -> `Reg(17)
| "C2"  -> `Reg(18)
| "C3"  -> `Reg(19)
| "C4"  -> `Reg(20)
| "C5"  -> `Reg(21)
| "C6"  -> `Reg(22)
| "C7"  -> `Reg(23)
| "C8"  -> `Reg(24)
| "C9"  -> `Reg(25)
| "C10" -> `Reg(26)
| "C11" -> `Reg(27)
| "C12" -> `Reg(28)
| "C13" -> `Reg(29)
| "C14" -> `Reg(30)
| "C15" -> `Reg(31)
| "C16" -> `Reg(32)
| "C17" -> `Reg(33)
| "C18" -> `Reg(34)
| "C19" -> `Reg(35)
| "C20" -> `Reg(36)
| "C21" -> `Reg(37)
| "C22" -> `Reg(38)
| "C23" -> `Reg(39)
| "C24" -> `Reg(40)
| "C25" -> `Reg(41)
| "C26" -> `Reg(42)
| "C27" -> `Reg(43)
| "C28" -> `Reg(44)
| "C29" -> `Reg(45)
| "C30" -> `Reg(46)
| "C31" -> `Reg(47)
| "I0"  -> `Reg(48)
| "I1"  -> `Reg(49)
| "I2"  -> `Reg(50)
| "I3"  -> `Reg(51)
| "S"   -> `Reg(52)
| "P"   -> `Reg(53)
| "HIT" -> `Reg(54)
| "A"   -> `Reg(55)
| "S0"  -> `Stack(0)
| "S1"  -> `Stack(1)
| "S2"  -> `Stack(2)
| "S3"  -> `Stack(3)
| "S4"  -> `Stack(4)
| "S5"  -> `Stack(5)
| "S6"  -> `Stack(6)
| "S7"  -> `Stack(7)
| "HIT_TRI" -> `HIT_TRI
| "HIT_OBJ" -> `HIT_OBJ
| "TRIADDR" -> `HIT_TRI
| "OBJADDR" -> `HIT_OBJ
| r     -> failwith (sprintf "Unknown register %s" r)

let target_register_to_string : target_register -> string
= function
|`Reg(r)   -> register_to_string r
|`Stack(i) -> sprintf "S%d" i

let source_register_to_string : source_register -> string
= function
|`Reg(r)   -> register_to_string r
|`Stack(i) -> sprintf "S%d" i
|`HIT_TRI  -> "HIT_TRI"
|`HIT_OBJ  -> "HIT_OBJ"


let source_to_string : source -> string
= function
|`Imm x        -> sprintf "%f" x
|`Reg(r,sw,sm) -> sprintf "%s%s%s" (sm_to_string sm) (source_register_to_string r) (sw_to_string sw)

let wb_to_target_string : writeback -> string
= fun (reg,wbm,_) ->
    sprintf "%s%s" (target_register_to_string reg) (wbm_to_string wbm)

let wb_to_imod_string : writeback -> string
= fun (_,_,imod) -> match imod with
|`NOP    -> ""
|`SAT    -> "_sat"
|`RCP(i) -> "_rcp" ^ (match i with 0->"x"|1->"y"|2->"z"|3->""|_->failwith "Internal error: rcp component index out of range 0..3")
|`RSQ(i) -> "_rsq" ^ (match i with 0->"x"|1->"y"|2->"z"|3->""|_->failwith "Internal error: rsq component index out of range 0..3")
