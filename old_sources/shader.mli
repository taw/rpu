(*
 * This format is used inside VM.
 * The asm should generate it.
 *
 * In the long term, more formats are going to be needed:
 * 1. format used internally in ASM
 * 2. hardware format
 * 3. optimized software raytracer format
 * 4. AJAX debugger format
 * 5. Interchange format between those 2,3,4
 *
 * This format is somewhere between 3,4, and 5
 *)

type imod = SAT | RCP | RSQ
type reg  = int (* 0..15 - R0..15 16 - S *)
type component = int (* 0..3 - X|Y|Z|W *)
type swizzle = (component*component*component*component)
type mask = (bool*bool*bool*bool)
(*
type smult = SM_HALF|SM_1|SM_2|SM_4
type smod = bool * smult (* negate? * multiply_by *)
*)
type smod = float (* Allowed values: 0.5 1.0 2.0 4.0 -0.5 -1.0 -2.0 -4.0 *)

type arg = Reg of string * (string option) * smod | Imm of float * smod

type cond_reduction = RED_AND | RED_OR
type cond0 = (bool*bool*bool) (* v<0 | 0<=v<1 | v>=1 *)
type cond = cond0 * mask * cond_reduction

type target = reg * mask
type source = SrcImm of float | SrcReg of (reg * swizzle * smod)

type label = int

type op = MOV | FRAC | ADD | MUL | MAD | DP3 | DP2H | DP4 | DP3H
type stmt_arith = op * imod option * target * source list

type stmt_secondary =
| CJMP   of cond * label
| RETURN of cond

type instruction = stmt_arith * stmt_secondary option

type shader = instruction array
