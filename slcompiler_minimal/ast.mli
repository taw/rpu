type typecast = [
| `FLOAT
| `STRING
| `COLOR  of string option
| `POINT  of string option
| `VECTOR of string option
| `NORMAL of string option
| `MATRIX of string option
]

type asg_op = [`EQ | `PLUSEQ | `MINUSEQ | `TIMESEQ | `DIVEQ ]
type texture_type = [`TEXTURE | `SHADOW | `ENVIRONMENT]

type relation = [
| `AND  of relation * relation
| `OR   of relation * relation
| `GT   of expr * expr
| `GE   of expr * expr
| `LT   of expr * expr
| `LE   of expr * expr
| `EQEQ of expr * expr
| `NE   of expr * expr
| `NEG  of relation
]
and asg_target = [`SCALAR of string | `ARRAYELT of string * expr]
(*and asgexp   = [`ASG of asg_target * asg_op * expr ] *)
(*and proccall = [ `PROCCALL of string * expr list ] *)
and expr = [
| `PLUS of expr * expr
| `MINUS of expr * expr
| `DOTPROD of expr * expr
| `CROSSPROD of expr * expr
| `TIMES of expr * expr
| `DIV of expr * expr
| `UMINUS of expr
| `MUX of relation * expr * expr
| `CAST of typecast * expr
| `ID of string
| `ARR of string * expr
| `STRING of string
| `FLOAT of float
| `TUPLE of expr list
| `TEXTURE of texture_type * expr * expr option * expr list
| `PROCCALL of string * expr list
| `ASG of asg_target * asg_op * expr
]
type asgexp = [`ASG of asg_target * asg_op * expr]
type proccall = [`PROCCALL of string * expr list]

(* Uniform/Varying/Default can be easily extracted from the parse tree,
 * but is completely ignored. This mean that some "illegal" constructs like:
 * varying float a;
 * uniform float b = a;
 * are allowed.
 *
 * The rules of `DEFAULT resolution
 * * local variables default to `VARYING
 * * function arguments default to `VARYING
 * * shader arguments default to `UNIFORM
 * * magic arguments default to whatever is in the specification
 *
 * The reason is that the compiled shaders are fundamentally one-ray-at-a-time,
 * so we don't gain anything from uniform shaders
 *
 * type uniform_or_varying = [`UNIFORM | `VARYING | `DEFAULT]
 *)

type vec_type = [`POINT|`VECTOR|`NORMAL]
type var_typespec = [`FLOAT | `STRING | `VEC of vec_type | `COLOR | `MATRIX]
type fun_typespec = [`VOID | var_typespec]

type formal_is_output = bool
type param = [`FORMAL of (formal_is_output * var_typespec * string * expr option)]

type statement = [
|`VARDEF of bool * var_typespec * string * expr option
|`RETURN of expr
|`IFELSE of relation * statement list * statement list
|`WHILE of relation * statement list
|`FOR of expr * relation * expr * statement list
|`SOLAR of expr list * statement list
|`ILLUMINATE  of expr list * statement list
|`ILLUMINANCE of expr list * statement list
|`BREAK of int
|`CONTINUE of int
|`BLOCK of statement list
| proccall
| asgexp
]

type shader_type = [`SURFACE | `IMAGER | `DISPLACEMENT | `VOLUME | `LIGHT]
type body  = statement list

type function_return_type = fun_typespec
type shader_definition   = [`SHADER of (shader_type * string * param list * body)]
type function_definition = [`FUNCTION of (function_return_type * string * param list * body)]
type definition = [shader_definition | function_definition]
