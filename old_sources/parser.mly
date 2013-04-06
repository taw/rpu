%{
open Ast
open Printf

let string_to_list str =
       let res = ref []
    in let _ = String.iter (
	fun char -> (res := char::!res)
    ) str
    in List.rev (!res)
let mask_parse str =
    List.map (
	function
	| 'x' -> X
	| 'y' -> Y
	| 'z' -> Z
	| 'w' -> W
	| c   -> failwith (sprintf "Swizzling mask %s contains invalid character (%c), only x, y, z, w are valid" str c)
    ) (string_to_list str)
(* No warnings *)
let warning _ = ()

let mask_D = function
    | Some mask ->
    (
       let pmask = mask_parse mask
       in let (x,y,z,w) = (ref false, ref false, ref false, ref false)
       in let _ = List.iter (
         function
	 | X -> if !x then warning (sprintf ".x appears more than once in writeback mask %s" mask) else (x:=true)
	 | Y -> if !y then warning (sprintf ".y appears more than once in writeback mask %s" mask) else (y:=true)
	 | Z -> if !z then warning (sprintf ".z appears more than once in writeback mask %s" mask) else (z:=true)
	 | W -> if !w then warning (sprintf ".w appears more than once in writeback mask %s" mask) else (w:=true)
       ) pmask
       in (!x,!y,!z,!w)
    )
    | None -> (true, true, true, true)
let mask_S = function
    | Some mask ->
    (
      match (mask_parse mask)
      with
      | []       -> (X, Y, Z, W)
      | [a]      -> (a, a, a, a)
      | [a;b]    -> (a, b, b, b)
      | [a;b;c]  -> (a, b, c, c)
      | [a;b;c;d]-> (a, b, c, d)
      | pmask    -> failwith(sprintf "Swizzling mask %s contains %d elements, 4 max" mask (List.length pmask))
    )
    | None -> (X, Y, Z, W)

let apply_amod num (negate,sm) =
    let num = num *. (match sm with SM_HALF -> 0.5 | SM_1 -> 1.0 | SM_2 -> 2.0 | SM_4 -> 4.0)
    in if negate
    then -1.0 *. num
    else num

let arg_D = function
	  | Reg (reg,swizzling_mask,amod) ->
				    ( match amod with
                    | (false,SM_1) -> (reg, mask_D swizzling_mask)
				    | _            -> failwith "Destination cannot be negated or multiplied"
				    )
	  | Imm (_,_) -> failwith "Numbers can be used only as sources, not as destinations"
let arg_S = function
          | Reg (reg,swizzling_mask,amod) -> SrcReg (reg, mask_S swizzling_mask,amod)
	  | Imm (num,amod) -> let num = apply_amod num amod in SrcImm(num)

let arg_I = function
          | Reg (reg,swizzling_mask,amod) -> 
		if (swizzling_mask <> None)
		  then failwith "Load instruction target cannot be masked" (* or can it ? *)
		else if (amod <> (false,SM_1))
		  then failwith "Destination cannot be negated or multiplied"
		else
		(
		  match reg with
		  | "I0" -> 0
		  | "I1" -> 1
		  | "I2" -> 2
		  | "I3" -> 3
		  | _    -> failwith(sprintf "Only registers I0, I1, I2, I3 can be memory load destinations, not %s" reg)
		)
	  | Imm (_,_) -> failwith "Numbers can be used only as sources, not as destinations"

let arg_A = function
	  | Reg (reg,swizzling_mask,amod) ->
		if (amod <> (false,SM_1))
		  then failwith "Memory address cannot be negated or multiplied" (* or can it ? would be silly *)
		else (
		  match (reg,swizzling_mask) with
		  | ("A",Some "x") -> 0
		  | ("A",Some "y") -> 1
		  | ("A",Some "z") -> 2
		  | ("A",Some "w") -> 3
		  | ("A0",None) -> 0
		  | ("A1",None) -> 0
		  | ("A2",None) -> 0
		  | ("A3",None) -> 0
		  | ("A0",Some mask) -> (failwith (sprintf "%s is a scalar, %s.%s incorrect" reg reg mask))
		  | ("A1",Some mask) -> (failwith (sprintf "%s is a scalar, %s.%s incorrect" reg reg mask))
		  | ("A2",Some mask) -> (failwith (sprintf "%s is a scalar, %s.%s incorrect" reg reg mask))
		  | ("A3",Some mask) -> (failwith (sprintf "%s is a scalar, %s.%s incorrect" reg reg mask))
		  | ("A",Some mask) -> (failwith (sprintf "%s.%s is incorrect, use A0, A1, A2, A3 (= A.x, A.y, A.z, A.w)" reg mask))
		  | ("A",None)   -> (failwith (sprintf "%s is a vector, use A0, A1, A2, A3 (= A.x, A.y, A.z, A.w)" reg))
		  | (_, Some mask) -> (failwith (sprintf "%s.%s cannot be used as an address, only A0, A1, A2, A3 (= A.x, A.y, A.z, A.w) are ok" reg mask))
		  | (_, None) -> (failwith (sprintf "%s cannot be used as an address, only A0, A1, A2, A3 (= A.x, A.y, A.z, A.w) are ok" reg))
		)
	  | Imm (_,_) -> failwith "Numbers can not be used as memory addresses"
let arg_N = function
	  | Reg _ -> failwith "Only integers can be used as memory offsets"
	  | Imm (num,amod) ->
	    let num = apply_amod num amod
	    in int_of_float num

let arg_list_DS args fnam =
    match args with
    | [a;b] -> (arg_D a, arg_S b)
    | _ -> failwith (sprintf "%s expects 2 arguments, %d given" fnam (List.length args))
let arg_list_DSS args fnam =
    match args with
    | [a;b;c] -> (arg_D a, arg_S b, arg_S c)
    | _ -> failwith (sprintf "%s expects 3 arguments, %d given" fnam (List.length args))
let arg_list_DSSS args fnam =
    match args with
    | [a;b;c;d] -> (arg_D a, arg_S b, arg_S c, arg_S d)
    | _ -> failwith (sprintf "%s expects 4 arguments, %d given" fnam (List.length args))
let arg_list_SS args fnam =
    match args with
    | [a;b] -> (arg_S a, arg_S b)
    | _ -> failwith (sprintf "%s expects 2 arguments, %d given" fnam (List.length args))
let arg_list_SSS args fnam =
    match args with
    | [a;b;c] -> (arg_S a, arg_S b, arg_S c)
    | _ -> failwith (sprintf "%s expects 3 arguments, %d given" fnam (List.length args))
let arg_list_IAN args fnam =
    match args with
    | [a;b;c] -> (arg_I a, arg_A b, arg_N c)
    | _ -> failwith (sprintf "%s expects 3 arguments, %d given" fnam (List.length args))
let arg_list_ISS args fnam =
    match args with
    | [a;b;c] -> (arg_I a, arg_S b, arg_S c)
    | _ -> failwith (sprintf "%s expects 3 arguments, %d given" fnam (List.length args))
let arg_list_ANS args fnam =
    match args with
    | [a;b;c] -> (arg_A a, arg_N b, arg_S c)
    | _ -> failwith (sprintf "%s expects 3 arguments, %d given" fnam (List.length args))
let arg_list_AN args fnam =
    match args with
    | [a;b] -> (arg_A a, arg_N b)
    | _ -> failwith (sprintf "%s expects 2 arguments, %d given" fnam (List.length args))
%}
%token K_PAR
%token<Ast.opcode_arith*(Ast.imod option)> OpCode
%token<Ast.opcode_special> OpCodeD
%token K_IF K_JMP K_CALL K_TRACE K_RETURN K_PUSH K_RTY K_EXIT
%token K_OR K_AND
%token PAREN_O PAREN_C
%token GE LT
%token PLUS MINUS COLON COMMA DOT STAR
%token<string> ID
%token<float> NUM
%token EOF

%type<Ast.stmt list> program
%start program

%%

program:
| statements EOF	{ $1 }
statements:
| /* */			{ [] }
| statement statements	{ $1::$2 }
statement:
| K_PAR PAREN_O statements PAREN_C { `PAR($3) }
| simple_statement		 { $1 }
| simple_statement PLUS simple_statement
				{
				  (
				    let a = match $1 with
				    | #Ast.stmt_arith_or_complex as i -> i
				    | _ -> failwith "The second instruction of a pair must be arithmetic or complex arithmetic"
				    in let b = match $3 with
				    | #Ast.stmt_arith_or_secondary as i -> i
				    | _ -> failwith "The second instruction of a pair must be arithmetic or secondary"
				    in `PAIR(a,b)
				  )
				}
simple_statement:
| ID COLON		      { `LABEL($1) }
| OpCode arg_list             { let (opc,imod) = $1 in
				match opc with 
				| `MOV  -> let a,b     = arg_list_DS   $2 "mov"  in `MOV(imod,a,b)
				| `FRAC -> let a,b     = arg_list_DS   $2 "frac" in `FRAC(imod,a,b)
				| `ADD  -> let a,b,c   = arg_list_DSS  $2 "add"  in `ADD(imod,a,b,c)
				| `MUL  -> let a,b,c   = arg_list_DSS  $2 "mul"  in `MUL(imod,a,b,c)
				| `MAD  -> let a,b,c,d = arg_list_DSSS $2 "mad"  in `MAD(imod,a,b,c,d)
				| `DP3  -> let a,b,c   = arg_list_DSS  $2 "dp3"  in `DP3(imod,a,b,c)
				| `DP2H -> let a,b,c   = arg_list_DSS  $2 "dp2h" in `DP2H(imod,a,b,c)
				| `DP4  -> let a,b,c   = arg_list_DSS  $2 "dp4"  in `DP4(imod,a,b,c)
				| `DP3H -> let a,b,c   = arg_list_DSS  $2 "dp3h" in `DP3H(imod,a,b,c)
			      }
| OpCodeD arg_list            {
				match $1 with
				| `LOAD     -> let a,b,c = arg_list_IAN $2 "load"      in `LOAD(a,b,c)
				| `LOAD4    -> let a,b   = arg_list_AN  $2 "load4x"    in `LOAD4(a,b)
				| `STORE    -> let a,b,c = arg_list_ANS $2 "store"     in `STORE(a,b,c)
				| `TEXLOAD  -> let a,b,c = arg_list_ISS $2 "texload"   in `TEXLOAD(a,b,c)
				| `TEXLOAD4 -> let a,b   = arg_list_SS  $2 "texload4x" in `TEXLOAD4(a,b)
				| `TEXSTORE -> let a,b,c = arg_list_SSS $2 "texstore"  in `TEXSTORE(a,b,c)
			      }
| K_RETURN condspec	      {
				`RETURN($2)
			      }
| K_JMP ID COMMA condspec     {
				`CJMP($4,$2)
			      }
| K_TRACE K_PUSH NUM K_RTY NUM{ `TRACE(int_of_float $3,int_of_float $5) }
| K_CALL K_PUSH NUM K_RTY NUM { `CALL(int_of_float $3,int_of_float $5) }
| K_EXIT NUM		      { `EXIT(int_of_float $2) }
condspec:
| orandopt ID cond		{ let m = mask_D (Some $2) in 
				  match $1 with
				  | Some oa -> ($3,m,oa)
				  | None -> let (mx,my,mz,mw) = m in
					    let s = (if mx then 1 else 0)
						  + (if my then 1 else 0)
						  + (if mz then 1 else 0)
						  + (if mw then 1 else 0)
					    in if s <= 1
					       then ($3,m,RED_AND)
					       else failwith(sprintf
					       "Empty condition can be used only for single-component checks, specify \"and %s\" or \"or %s\""
					       $2 $2
					       )
				}
orandopt:
| /* */			{ None }
| K_OR			{ Some RED_OR }
| K_AND			{ Some RED_AND }
cond:
| cond1			{ $1 }
| PAREN_O cond2 PAREN_C	{ $2 }
| PAREN_O cond1 PAREN_C	{ $2 }
cond2:
| cond1 K_AND cond1	{
			  let (aNeg,aSmall,aBig) = $1 in
			  let (bNeg,bSmall,bBig) = $3 in
			  ((aNeg && bNeg), (aSmall && bSmall), (aBig && bBig))
			}
| cond1 K_OR cond1	{
			  let (aNeg,aSmall,aBig) = $1 in
			  let (bNeg,bSmall,bBig) = $3 in
			  ((aNeg || bNeg), (aSmall || bSmall), (aBig || bBig))
			}
cond1:
| GE NUM		{
			  if $2 = 0.0
                            then (false,true,true)
			  else if $2 = 1.0
			    then (false,false,true)
			  else
			    failwith(sprintf "Only comparisons with 0 and 1 supported, not %f" $2)
			}
| LT NUM		{ 
			  if $2 = 0.0
                            then (true,false,false)
			  else if $2 = 1.0
			    then (true,true,false)
			  else
			    failwith(sprintf "Only comparisons with 0 and 1 supported, not %f" $2)
			}
arg_list:
| arg			{ $1::[] }
| arg COMMA arg_list	{ $1::$3 }
arg:
| opt_minus opt_mult ID	{ Reg($3,None,($1,$2)) }
| opt_minus opt_mult ID DOT ID	{ Reg($3,Some $5,($1,$2)) }
| opt_minus NUM		{ Imm($2,($1,SM_1)) }
opt_minus:
| /* */			{ false }
| MINUS			{ true }
opt_mult:
| /* */ 		{ SM_1 }
| NUM STAR		{ match $1 with
			  | 0.5 -> SM_HALF
			  | 1.0 -> SM_1
			  | 2.0 -> SM_2
			  | 4.0 -> SM_4
			  | x   -> failwith(sprintf "The only valid source multipliers are 0.5, 1.0, 2.0 and 4.0, not %f" x)
			}
