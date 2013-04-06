open Util

(*
type label = STATEMENT_INDEX of int
*)

type vec_type = [`POINT | `NORMAL | `VECTOR]
type var_typespec = [`FLOAT | `STRING | `VEC of vec_type | `COLOR | `MATRIX]
type fun_typespec = [`VOID  | var_typespec ]

(* TODO: make *_comp support everything *)
type flt_temp = [`FLT_TEMP of int]
type vec_temp = [`VEC_TEMP of int * vec_type]
type mtx_temp = [`MTX_TEMP of int]
type col_temp = [`COL_TEMP of int]
type str_temp = [`STR_TEMP of int]
type any_temp = [
| flt_temp
| vec_temp
| mtx_temp
| col_temp
| str_temp
]
type tuple = flt_temp list
type fun_call = string * any_temp list
type flt_comp = [
|`F_CONST of float
|`F_ASG of flt_temp
|`F_FUN of fun_call
|`F_ARR_LD of string * flt_temp
|`F_ADD of flt_temp * flt_temp
|`F_SUB of flt_temp * flt_temp
|`F_MUL of flt_temp * flt_temp
|`F_RCP of flt_temp
|`F_RSQ of flt_temp
|`F_DOT_VV of vec_temp * vec_temp
|`F_DOT_CC of col_temp * col_temp
]
type vec_comp = [
|`V_TUPLE of tuple (* 3-element only *)
|`V_ASG of vec_temp
|`V_FUN of fun_call
|`V_ARR_LD of string * flt_temp
|`V_FLTCAST of flt_temp
|`V_ADD of vec_temp * vec_temp
|`V_SUB of vec_temp * vec_temp
|`V_MUL of vec_temp * vec_temp
|`V_XPD of vec_temp * vec_temp
]
type col_comp = [
|`C_TUPLE of tuple (* TODO: number of elements should be a compiler flag (but isn't, only 3 is allowed) *)
|`C_ASG of col_temp
|`C_FUN of fun_call
|`C_ARR_LD of string * flt_temp
|`C_FLTCAST   of flt_temp
|`C_ADD of col_temp * col_temp
|`C_SUB of col_temp * col_temp
|`C_MUL of col_temp * col_temp
]
type mtx_comp = [
|`M_TUPLE of tuple (* 16-elements only *)
|`M_ASG of mtx_temp
|`M_FLTCAST of flt_temp
|`M_FUN of fun_call
|`M_ARR_LD of string * flt_temp
|`M_ADD of mtx_temp * mtx_temp
|`M_SUB of mtx_temp * mtx_temp
|`M_MUL of mtx_temp * mtx_temp
]
type str_comp = [
|`S_CONST of string
|`S_ASG of str_temp
|`S_FUN of fun_call
|`S_ARR_LD of string * flt_temp
]

type cond = [
|`GT  of flt_temp * flt_temp
|`GE  of flt_temp * flt_temp
|`FEQ of flt_temp * flt_temp
|`VEQ of vec_temp * vec_temp
|`CEQ of col_temp * col_temp
|`MEQ of mtx_temp * mtx_temp
|`SEQ of str_temp * str_temp
]

type link_out = int * (any_temp list)
type link_in  = any_temp list

type op = [
| `NOP (* do nothing *)
| `CJMP of cond * link_out
| `FCOMP of flt_temp * flt_comp 
| `VCOMP of vec_temp * vec_comp 
| `CCOMP of col_temp * col_comp 
| `MCOMP of mtx_temp * mtx_comp 
| `SCOMP of str_temp * str_comp 
| `PROCCALL of fun_call
]

type statement_cnt = (link_in * op * link_out)

type semantic_info = [
| `MAGIC  of string
| `LOCAL
| `PARAM  of int
]
(* FIXME: is_extern, is_array, basic_type, sem_info *)
type vardef = string * (bool * bool * var_typespec * semantic_info)
(* statements, local variables, start label, end label *)
type body = ((int,statement_cnt) ht) * vardef list * int * int

type formal_is_output = bool
type param_default_value = Ast.expr (* FIXME *)
type param = [`FORMAL of (formal_is_output * var_typespec * string * param_default_value option) ]

type shader_type = [`SURFACE | `IMAGER | `DISPLACEMENT | `VOLUME | `LIGHT ]

(*
 * Initial temp feed
 * parameters * extern/output/magic
 *)
type prelude_element = [
| `EXTERN of string * any_temp
| `MAGIC  of string * any_temp
]
type prelude = any_temp list * prelude_element list

(*
 * Temp writeback at exit
 * return_value * extern/output/magic
 *)
type retval   = any_temp option
type epilogue_element = [
| `EXTERN of string * any_temp
| `OUTPUT of int    * any_temp
| `MAGIC  of string * any_temp
]
type epilogue = any_temp list * retval * epilogue_element list

type function_return_type = fun_typespec
type shader_definition   = [`SHADER of (shader_type * string * param list * prelude * body * epilogue) ]
type function_definition = [`FUNCTION of (function_return_type * string * param list * prelude * body * epilogue) ]
type definition = [shader_definition | function_definition]
