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

val stmt_to_string : stmt -> string
val print_ast : stmt list -> unit
