(* These are used to communicate between the lexer and the parser *)
type opcode_arith = [`MOV | `FRAC | `ADD | `MUL | `MAD | `DP3 | `DP2H | `DP4 | `DP3H]
type opcode_special = [`LOAD | `LOAD4 | `STORE | `TEXLOAD | `TEXLOAD4 | `TEXSTORE]

(* These are used to communicate between the parser and the output functions *)
type imod = SAT | RCP | RSQ
type reg = string (* R0..R15 S0..S7 C0..C31 S I0..I3 A etc. *)
type component = X | Y | Z | W
type swizzle = (component*component*component*component)
type mask = (bool*bool*bool*bool)
type load_target = int (* 0..3 for I0 I1 I2 I3 *)
type addr = int (* 0..3 for A0 A1 A2 A3 or A.x A.y A.z A.w *)
type memshift = int (* limited to a few bits in the real hardware *)
type smult = SM_HALF|SM_1|SM_2|SM_4
type smod = bool * smult (* negate? * multiply_by *)

(* These one is used inside a parser *)
type arg = Reg of string * (string option) * smod | Imm of float * smod

type cond_reduction = RED_AND | RED_OR
type cond0 = (bool*bool*bool) (* v<0 | 0<=v<1 | v<=1 *)
type cond = cond0 * mask * cond_reduction

type target = reg * mask
type source = SrcImm of float | SrcReg of (reg * swizzle * smod)

type label = string
type pushsize = int (* >=0 *)
type table    = int (* ??? *)
type exitcode = int

type stmt_arith =
[
|`MOV  of (imod option*target*source)
|`FRAC of (imod option*target*source)
|`ADD  of (imod option*target*source*source)
|`MUL  of (imod option*target*source*source)
|`MAD  of (imod option*target*source*source*source)
]

type stmt_complex =
[
|`DP3  of (imod option*target*source*source)
|`DP2H of (imod option*target*source*source)
|`DP4  of (imod option*target*source*source)
|`DP3H of (imod option*target*source*source)
]

type stmt_secondary =
[
|`LOAD   of load_target * addr * memshift
|`LOAD4  of addr * memshift
|`CJMP   of cond * label
(*
| CCALL  of cond * label * pushsize * (table option)
*)
|`CALL   of pushsize * table
|`TRACE  of pushsize * table
|`RETURN of cond
|`EXIT   of exitcode
]

type stmt_arith_or_complex   = [stmt_arith | stmt_complex]
type stmt_arith_or_secondary = [stmt_arith | stmt_secondary]

(* TODO: special should be compiled to texture loads
         or something like that.
         They cannot be grouped.
*)
type isaext_special = [`SIN | `COS | `EXP | `LOG]

type stmt_single =
[
|`STORE    of (addr * memshift * source)
|`TEXLOAD  of (load_target * source * source)
|`TEXLOAD4 of (source * source)
|`TEXSTORE of (source * source * source)
|`SPECIAL  of (isaext_special * imod option * target * source)
]

type stmt =
[
| stmt_arith
| stmt_complex
| stmt_secondary
| stmt_single
| `PAIR of (stmt_arith_or_complex * stmt_arith_or_secondary)
| `LABEL of label
(* parallel computations *)
| `PAR of stmt list
]

(* IMPLEMENTATION *)

open Printf
(*
 * Ast printing
 *)

let loadtarget_to_string lt = sprintf "L%d" lt
let memshift_to_string n = sprintf "%d" n
let addr_to_string a = sprintf "A%d" a

let imod_to_string = function
| None     -> ""
| Some SAT -> "_sat"
| Some RCP -> "_rcp"
| Some RSQ -> "_rsq"

let sw_to_string (xsw,ysw,wsw,zsw) =
    let sw1 = function
    | X -> "x"
    | Y -> "y"
    | Z -> "z"
    | W -> "w"
 in if (xsw,ysw,wsw,zsw) = (X,Y,Z,W)
    then ""
    else "."^(sw1 xsw)^(sw1 ysw)^(sw1 zsw)^(sw1 wsw)

let sm_to_string (neg,smult) =
    (if neg then "-" else "")^
    (match smult with
    | SM_HALF -> "0.5*"
    | SM_1    -> ""
    | SM_2    -> "2*"
    | SM_4    -> "4*"
    )

let wbm_to_string (xm,ym,zm,wm) =
    if (xm&&ym&&zm&&wm)
    then ""
    else
    "."^
    (if xm then "x" else "")^
    (if ym then "y" else "")^
    (if zm then "z" else "")^
    (if wm then "w" else "")


let cond_to_string (c0,(xm,ym,zm,wm),cond_reduction) = 
    (match cond_reduction with RED_OR -> "or" | RED_AND -> "and")^
    " "^
    (if xm then "x" else "")^
    (if ym then "y" else "")^
    (if zm then "z" else "")^
    (if wm then "w" else "")^
    (if xm||ym||zm||wm then "" else failwith "at least one of the mask elements should be true")^
    " "^
    (match c0 with
    | ( true, true, true) -> "(>=0 or <1)" (* always *)
    | ( true, true,false) -> "<1"
    | ( true,false, true) -> "(<0 or >=1)"
    | ( true,false,false) -> "<0"
    | (false, true, true) -> ">=0"
    | (false, true,false) -> "(>=0 and <1)"
    | (false,false, true) -> ">=1"
    | (false,false,false) -> "(<0 and >=1)" (* never *)
    )

let target_to_string (reg, wbm) = sprintf "%s%s" reg (wbm_to_string wbm)
let source_to_string = function
| SrcImm x        -> sprintf "%f" x
| SrcReg(r,sw,sm) -> sprintf "%s%s%s" (sm_to_string sm) r (sw_to_string sw)

let isaext_special_to_string = function
|`SIN -> "sin"
|`COS -> "cos"
|`EXP -> "exp"
|`LOG -> "log"

let rec (stmt_to_string : stmt -> string) = function
|`MOV(im,t,s)        -> sprintf "mov%s %s, %s\n" (imod_to_string im) (target_to_string t) (source_to_string s)
|`FRAC(im,t,s)       -> sprintf "frac%s %s, %s\n" (imod_to_string im) (target_to_string t) (source_to_string s)
|`ADD(im,t,s1,s2)    -> sprintf "add%s %s, %s, %s\n" (imod_to_string im) (target_to_string t) (source_to_string s1) (source_to_string s2)
|`MUL(im,t,s1,s2)    -> sprintf "mul%s %s, %s, %s\n" (imod_to_string im) (target_to_string t) (source_to_string s1) (source_to_string s2)
|`MAD(im,t,s1,s2,s3) -> sprintf "mad%s %s, %s, %s, %s\n" (imod_to_string im) (target_to_string t) (source_to_string s1) (source_to_string s2) (source_to_string s3)
|`DP3(im,t,s1,s2)    -> sprintf "dp3%s %s, %s, %s\n" (imod_to_string im) (target_to_string t) (source_to_string s1) (source_to_string s2)
|`DP2H(im,t,s1,s2)   -> sprintf "dp2h%s %s, %s, %s\n" (imod_to_string im) (target_to_string t) (source_to_string s1) (source_to_string s2)
|`DP4(im,t,s1,s2)    -> sprintf "dp4%s %s, %s, %s\n" (imod_to_string im) (target_to_string t) (source_to_string s1) (source_to_string s2)
|`DP3H(im,t,s1,s2)   -> sprintf "dp3h%s %s, %s, %s\n" (imod_to_string im) (target_to_string t) (source_to_string s1) (source_to_string s2)
|`LOAD(lt,a,m)       -> sprintf "load4 %s,%s,%s\n" (loadtarget_to_string lt) (addr_to_string a) (memshift_to_string m)
|`LOAD4(a,m)         -> sprintf "load4 %s,%s\n" (addr_to_string a) (memshift_to_string m)
|`CJMP(c,l)          -> sprintf "jmp %s, %s\n" l (cond_to_string c)
|`CALL(p,t)          -> sprintf "call %d, %d\n"  p t
|`TRACE(p,t)         -> sprintf "trace %d, %d\n" p t
|`RETURN(c)          -> sprintf "return %s\n" (cond_to_string c)
|`EXIT(e)            -> sprintf "exit %d\n" e
|`STORE(a,m,s)       -> sprintf "store %s, %s, %s\n"    (addr_to_string a) (memshift_to_string m) (source_to_string s)
|`TEXLOAD(lt,s1,s2)  -> sprintf "texload %s, %s, %s\n"  (loadtarget_to_string lt) (source_to_string s1) (source_to_string s2)
|`TEXLOAD4(s1,s2)    -> sprintf "texload4 %s, %s\n"     (source_to_string s1) (source_to_string s2)
|`TEXSTORE(s1,s2,s3) -> sprintf "texstore %s, %s, %s\n" (source_to_string s1) (source_to_string s2) (source_to_string s3)
|`SPECIAL(x,im,t,s)  -> sprintf "%s%s %s, %s\n" (isaext_special_to_string x) (imod_to_string im) (target_to_string t) (source_to_string s)
|`PAIR(i1,i2)        -> (stmt_to_string (i1 :> stmt))   ^ "\n+ " ^ (stmt_to_string (i2 :> stmt)) ^ "\n"
|`LABEL(lab)         -> sprintf "%s:\n" lab
|`PAR(ss)            -> sprintf "par (\n%s)\n" (List.fold_left (fun s i -> s^(stmt_to_string i)) "" ss)

let print_ast = List.iter (fun x -> printf "%s" (stmt_to_string  x))
