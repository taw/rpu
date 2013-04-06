(* These are used to communicate between the lexer and the parser
 * The difference between the two classes is syntactical
 * opcode_arith can carry modifiers (like mov_sat dp3_rsq etc.)
 * opcode_special can not
 *)
type opcode_arith = [`MOV | `FRAC | `ADD | `MUL | `MAD | `DP3 | `DP2H | `DP4 | `DP3H]
type opcode_special = [`LOAD | `LOAD4 | `STORE | `TEXLOAD | `TEXLOAD4 | `TEXSTORE | `TRACE]

(* These are used to communicate between the parser and the output functions *)
type imod = Opcodes.imod
type reg = string (* R0..R15 S0..S7 C0..C31 S I0..I3 A etc. *)
type swizzle = Opcodes.swizzle
type load_target = int (* 0..3 for I0 I1 I2 I3 *)
type addr = int (* 0..3 for A0 A1 A2 A3 or A.x A.y A.z A.w *)
type memshift = int (* limited to a few bits in the real hardware *)
type smult = Opcodes.smult

(* This one is used inside a parser *)
type arg = Reg of string * (string option) * smult | Imm of float

type cond = Opcodes.cond

type label = string
type pushsize = int (* >=0 *)
type table    = int (* ??? *)
type exitcode = int

type source    = Opcodes.source
type writeback_mask = Opcodes.writeback_mask
type writeback = Opcodes.writeback

type stmt_arith =
[
|`MOV  of writeback * source
|`FRAC of writeback * source
|`ADD  of writeback * source * source
|`MUL  of writeback * source * source
|`MAD  of writeback * source * source * source
]

type stmt_complex =
[
|`DP3  of writeback * source * source
|`DP2H of writeback * source * source
|`DP4  of writeback * source * source
|`DP3H of writeback * source * source
]

type stmt_secondary =
[
|`LOAD   of load_target * addr * memshift
|`LOAD4  of addr * memshift
|`CJMP   of cond * label
|`JMP    of label
(*
| CCALL  of cond * label * pushsize * (table option)
|`CALL   of pushsize * table
*)
|`CALL   of label * pushsize
|`DCALL  of pushsize (* call HIT.w *)
(*
|`TRACE  of pushsize * table
*)
|`TRACE  of source*source*source
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
|`SPECIAL  of (isaext_special * writeback * source)
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
(*************************************
 * Ast printing                      *
 *************************************
 *)

let loadtarget_to_string lt = sprintf "L%d" lt
let memshift_to_string n = sprintf "%d" n
let addr_to_string a = sprintf "A%d" a

let isaext_special_to_string = function
|`SIN -> "sin"
|`COS -> "cos"
|`EXP -> "exp"
|`LOG -> "log"

let cond_to_string      = Opcodes.cond_to_string
let wb_to_imod_string   = Opcodes.wb_to_imod_string
let wb_to_target_string = Opcodes.wb_to_target_string
let source_to_string    = Opcodes.source_to_string

let rec (stmt_to_string : stmt -> string) = function
|`MOV(wb,s)          -> sprintf "mov%s %s, %s\n"         (wb_to_imod_string wb) (wb_to_target_string wb) (source_to_string s)
|`FRAC(wb,s)         -> sprintf "frac%s %s, %s\n"        (wb_to_imod_string wb) (wb_to_target_string wb) (source_to_string s)
|`ADD(wb,s1,s2)      -> sprintf "add%s %s, %s, %s\n"     (wb_to_imod_string wb) (wb_to_target_string wb) (source_to_string s1) (source_to_string s2)
|`MUL(wb,s1,s2)      -> sprintf "mul%s %s, %s, %s\n"     (wb_to_imod_string wb) (wb_to_target_string wb) (source_to_string s1) (source_to_string s2)
|`MAD(wb,s1,s2,s3)   -> sprintf "mad%s %s, %s, %s, %s\n" (wb_to_imod_string wb) (wb_to_target_string wb) (source_to_string s1) (source_to_string s2) (source_to_string s3)
|`DP3(wb,s1,s2)      -> sprintf "dp3%s %s, %s, %s\n"     (wb_to_imod_string wb) (wb_to_target_string wb) (source_to_string s1) (source_to_string s2)
|`DP2H(wb,s1,s2)     -> sprintf "dp2h%s %s, %s, %s\n"    (wb_to_imod_string wb) (wb_to_target_string wb) (source_to_string s1) (source_to_string s2)
|`DP4(wb,s1,s2)      -> sprintf "dp4%s %s, %s, %s\n"     (wb_to_imod_string wb) (wb_to_target_string wb) (source_to_string s1) (source_to_string s2)
|`DP3H(wb,s1,s2)     -> sprintf "dp3h%s %s, %s, %s\n"    (wb_to_imod_string wb) (wb_to_target_string wb) (source_to_string s1) (source_to_string s2)
|`LOAD(lt,a,m)       -> sprintf "load4 %s,%s,%s\n" (loadtarget_to_string lt) (addr_to_string a) (memshift_to_string m)
|`LOAD4(a,m)         -> sprintf "load4 %s,%s\n" (addr_to_string a) (memshift_to_string m)
|`CJMP(c,l)          -> sprintf "jmp %s, %s\n" l (cond_to_string c)
|`JMP(l)             -> sprintf "jmp %s\n" l
|`CALL(l,psz)        -> sprintf "call %s push %d\n" l psz
|`DCALL(psz)         -> sprintf "call HIT.w push %d\n" psz
|`TRACE(s1,s2,s3)    -> sprintf "trace %s, %s, %s\n" (source_to_string s1) (source_to_string s2) (source_to_string s3)
|`RETURN(c)          -> sprintf "return %s\n" (cond_to_string c)
|`EXIT(e)            -> sprintf "exit %d\n" e
|`STORE(a,m,s)       -> sprintf "store %s, %s, %s\n"    (addr_to_string a) (memshift_to_string m) (source_to_string s)
|`TEXLOAD(lt,s1,s2)  -> sprintf "texload %s, %s, %s\n"  (loadtarget_to_string lt) (source_to_string s1) (source_to_string s2)
|`TEXLOAD4(s1,s2)    -> sprintf "texload4 %s, %s\n"     (source_to_string s1) (source_to_string s2)
|`TEXSTORE(s1,s2,s3) -> sprintf "texstore %s, %s, %s\n" (source_to_string s1) (source_to_string s2) (source_to_string s3)
|`SPECIAL(x,wb,s)    -> sprintf "%s%s %s, %s\n" (isaext_special_to_string x) (wb_to_imod_string wb) (wb_to_target_string wb) (source_to_string s)
|`PAIR(i1,i2)        -> (stmt_to_string (i1 :> stmt))   ^ "+ " ^ (stmt_to_string (i2 :> stmt))
|`LABEL(lab)         -> sprintf "%s:\n" lab
|`PAR(ss)            -> sprintf "par (\n%s)\n" (List.fold_left (fun s i -> s^(stmt_to_string i)) "" ss)

let print_ast = List.iter (fun x -> printf "%s" (stmt_to_string  x))
