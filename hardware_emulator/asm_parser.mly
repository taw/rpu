%{
open Asm_ast
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
	| 'x' -> 0
	| 'y' -> 1
	| 'z' -> 2
	| 'w' -> 3
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
	 | 0 -> if !x then warning (sprintf ".x appears more than once in writeback mask %s" mask) else (x:=true)
	 | 1 -> if !y then warning (sprintf ".y appears more than once in writeback mask %s" mask) else (y:=true)
	 | 2 -> if !z then warning (sprintf ".z appears more than once in writeback mask %s" mask) else (z:=true)
	 | 3 -> if !w then warning (sprintf ".w appears more than once in writeback mask %s" mask) else (w:=true)
	 | _ -> failwith "Internal error: writeback index out of 0..3 range"
       ) pmask
       in {Opcodes.wb_x = !x; Opcodes.wb_y = !y; Opcodes.wb_z = !z; Opcodes.wb_w = !w}
    )
    | None -> {Opcodes.wb_x = true; Opcodes.wb_y = true; Opcodes.wb_z = true; Opcodes.wb_w = true;}
let mask_S = function
    | Some mask ->
    (
      match (mask_parse mask)
      with
      | []       -> {Opcodes.sw_x=0; Opcodes.sw_y=1; Opcodes.sw_z=2; Opcodes.sw_w=3}
      | [a]      -> {Opcodes.sw_x=a; Opcodes.sw_y=a; Opcodes.sw_z=a; Opcodes.sw_w=a}
      | [a;b]    -> {Opcodes.sw_x=a; Opcodes.sw_y=b; Opcodes.sw_z=b; Opcodes.sw_w=b}
      | [a;b;c]  -> {Opcodes.sw_x=a; Opcodes.sw_y=b; Opcodes.sw_z=c; Opcodes.sw_w=c}
      | [a;b;c;d]-> {Opcodes.sw_x=a; Opcodes.sw_y=b; Opcodes.sw_z=c; Opcodes.sw_w=d}
      | pmask    -> failwith(sprintf "Swizzling mask %s contains %d elements, 4 max" mask (List.length pmask))
    )
    | None -> {Opcodes.sw_x=0; Opcodes.sw_y=1; Opcodes.sw_z=2; Opcodes.sw_w=3}

let arg_D = function
	  | Reg (reg,swizzling_mask,smult) ->
				    ( match smult with
                                    | 1.0 -> (reg, mask_D swizzling_mask)
			            | _            -> failwith "Destination cannot be negated or multiplied"
			            )
	  | Imm _ -> failwith "Numbers can be used only as sources, not as destinations"
let arg_S = function
          | Reg (reg,swizzling_mask,smult) -> `Reg(Opcodes.compile_regname reg, mask_S swizzling_mask,smult)
	  | Imm num -> `Imm(num)
let arg_I = function
          | Reg (reg,swizzling_mask,smult) -> 
		if (swizzling_mask <> None)
		  then failwith "Load instruction target cannot be masked" (* or can it ? *)
		else if (smult <> 1.0)
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
	  | Imm _ -> failwith "Numbers can be used only as sources, not as destinations"

let arg_A = function
	  | Reg (reg,swizzling_mask,smult) ->
		if (smult <> 1.0)
		  then failwith "Memory address cannot be negated or multiplied" (* or can it ? would be silly *)
		else (
		  match (reg,swizzling_mask) with
		  | ("A",Some "x")  -> 0
		  | ("A",Some "y")  -> 1
		  | ("A",Some "z")  -> 2
		  | ("A",Some "w")  -> 3
		  | ("A0",None)     -> 0
		  | ("A1",None)     -> 1
		  | ("A2",None)     -> 2
		  | ("A3",None)     -> 3
		  | ("A0",Some mask)-> (failwith (sprintf "%s is a scalar, %s.%s incorrect" reg reg mask))
		  | ("A1",Some mask)-> (failwith (sprintf "%s is a scalar, %s.%s incorrect" reg reg mask))
		  | ("A2",Some mask)-> (failwith (sprintf "%s is a scalar, %s.%s incorrect" reg reg mask))
		  | ("A3",Some mask)-> (failwith (sprintf "%s is a scalar, %s.%s incorrect" reg reg mask))
		  | ("A",Some mask) -> (failwith (sprintf "%s.%s is incorrect, use A0, A1, A2, A3 (= A.x, A.y, A.z, A.w)" reg mask))
		  | ("A",None)      -> (failwith (sprintf "%s is a vector, use A0, A1, A2, A3 (= A.x, A.y, A.z, A.w)" reg))
		  | ("TRIADDR",None)-> 4
		  | ("OBJADDR",None)-> 5
		  | ("HIT_TRI",None)-> 4
		  | ("HIT_OBJ",None)-> 5
		  | (_, Some mask)  -> (failwith (sprintf "%s.%s cannot be used as an address, only A0, A1, A2, A3 (= A.x, A.y, A.z, A.w), TRIADDR/HIT_TRI, OBJADDR/HIT_OBJ are ok" reg mask))
		  | (_, None)       -> (failwith (sprintf "%s cannot be used as an address, only A0, A1, A2, A3 (= A.x, A.y, A.z, A.w), TRIADDR/HIT_TRI, OBJADDR/HIT_OBJ are ok" reg))
		)
	  | Imm _ -> failwith "Numbers can not be used as memory addresses"
let arg_N = function
	  | Reg _ -> failwith "Only integers can be used as memory offsets"
	  | Imm num -> int_of_float num

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

let compile_wb : imod -> (string * writeback_mask) -> writeback
= fun imod (reg_name, wbm) ->
    ((match Opcodes.compile_regname reg_name with
        | (#Opcodes.target_register as reg) -> reg
        | `HIT_TRI -> failwith "Input error: HIT_TRI cannot be used as a target register"
        | `HIT_OBJ -> failwith "Input error: HIT_OBJ cannot be used as a target register"
    ), wbm, imod)

%}
%token K_PAR
%token<Asm_ast.opcode_arith*Asm_ast.imod> OpCode
%token<Asm_ast.opcode_special> OpCodeD
%token K_IF K_JMP K_CALL K_RETURN K_PUSH K_RTY K_EXIT
%token K_OR K_AND
%token PAREN_O PAREN_C
%token GE GT LE LT EQ NE
%token PLUS MINUS COLON COMMA DOT STAR
%token<string> ID
%token<float> NUM
%token EOF

%type<Asm_ast.stmt list> program
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
				    | #Asm_ast.stmt_arith_or_complex as i -> i
				    | _ -> failwith "The second instruction of a pair must be arithmetic or complex arithmetic"
				    in let b = match $3 with
				    | #Asm_ast.stmt_arith_or_secondary as i -> i
				    | _ -> failwith "The second instruction of a pair must be arithmetic or secondary"
				    in `PAIR(a,b)
				  )
				}
simple_statement:
| ID COLON		      { `LABEL($1) }
| NUM COLON		      {  (* Not very nice. Anyway, both foo: and 2: labels are acceptable, don't try using floating point label like 2.5 *)
                                `LABEL(string_of_int(int_of_float $1)) 
                              }
| OpCode arg_list             { let (opc,imod) = $1 in
				match opc with 
				| `MOV  -> let a,b     = arg_list_DS   $2 "mov"  in
				           let wb      = compile_wb imod a       in `MOV (wb,b)
				| `FRAC -> let a,b     = arg_list_DS   $2 "frac" in
				           let wb      = compile_wb imod a       in `FRAC(wb,b)
				| `ADD  -> let a,b,c   = arg_list_DSS  $2 "add"  in 
				           let wb      = compile_wb imod a       in `ADD (wb,b,c)
				| `MUL  -> let a,b,c   = arg_list_DSS  $2 "mul"  in 
				           let wb      = compile_wb imod a       in `MUL (wb,b,c)
				| `MAD  -> let a,b,c,d = arg_list_DSSS $2 "mad"  in 
				           let wb      = compile_wb imod a       in `MAD (wb,b,c,d)
				| `DP3  -> let a,b,c   = arg_list_DSS  $2 "dp3"  in 
				           let wb      = compile_wb imod a       in `DP3 (wb,b,c)
				| `DP2H -> let a,b,c   = arg_list_DSS  $2 "dp2h" in 
				           let wb      = compile_wb imod a       in `DP2H(wb,b,c)
				| `DP4  -> let a,b,c   = arg_list_DSS  $2 "dp4"  in
				           let wb      = compile_wb imod a       in `DP4 (wb,b,c)
				| `DP3H -> let a,b,c   = arg_list_DSS  $2 "dp3h" in
				           let wb      = compile_wb imod a       in `DP3H(wb,b,c)
			      }
| OpCodeD arg_list            {
				match $1 with
				| `LOAD     -> let a,b,c = arg_list_IAN $2 "load"      in `LOAD(a,b,c)
				| `LOAD4    -> let a,b   = arg_list_AN  $2 "load4x"    in `LOAD4(a,b)
				| `STORE    -> let a,b,c = arg_list_ANS $2 "store"     in `STORE(a,b,c)
				| `TEXLOAD  -> let a,b,c = arg_list_ISS $2 "texload"   in `TEXLOAD(a,b,c)
				| `TEXLOAD4 -> let a,b   = arg_list_SS  $2 "texload4x" in `TEXLOAD4(a,b)
				| `TEXSTORE -> let a,b,c = arg_list_SSS $2 "texstore"  in `TEXSTORE(a,b,c)
				| `TRACE    -> let a,b,c = arg_list_SSS $2 "trace"     in `TRACE(a,b,c)
			      }
| K_RETURN condspec	      {
				`RETURN($2)
			      }
| K_JMP ID COMMA condspec     {
				`CJMP($4,$2)
			      }
| K_JMP ID                    {
				`JMP($2)
			      }
| K_CALL ID K_PUSH NUM        {
				`CALL($2,int_of_float $4)
			      }
| K_CALL ID DOT ID K_PUSH NUM { (* Ugly dack *)
                                if $2 = "HIT" && $4 = "w"
				then `DCALL(int_of_float $6)
                                else failwith "Input error: CALL must either use a label or HIT.w"
			      }
/*
| K_TRACE K_PUSH NUM K_RTY NUM{ `TRACE(int_of_float $3,int_of_float $5) }
| K_CALL K_PUSH NUM K_RTY NUM { `CALL(int_of_float $3,int_of_float $5) }
*/
| K_EXIT NUM		      { `EXIT(int_of_float $2) }
condspec:
| orandopt ID cond		{ let m = mask_D (Some $2) in 
                                  let (c_and, (lt0,eq0,b01,ge1)) =
				  (
				  match $1 with
				  | Some oa -> (oa,$3)
				  | None -> let s = (if m.Opcodes.wb_x then 1 else 0)
						  + (if m.Opcodes.wb_y then 1 else 0)
						  + (if m.Opcodes.wb_z then 1 else 0)
						  + (if m.Opcodes.wb_w then 1 else 0)
					    in if s <= 1
					       then (true,$3)
					       else failwith(sprintf
					       "Empty condition can be used only for single-component checks, specify either \"and %s\" or \"or %s\""
					       $2 $2
					       )
			          ) in
				  {
				    Opcodes.c_and = c_and;
				    Opcodes.c_lt0 = lt0;
                                    Opcodes.c_eq0 = eq0;
				    Opcodes.c_b01 = b01;
				    Opcodes.c_ge1 = ge1;
				    Opcodes.c_x   = m.Opcodes.wb_x;
				    Opcodes.c_y   = m.Opcodes.wb_y;
				    Opcodes.c_z   = m.Opcodes.wb_z;
				    Opcodes.c_w   = m.Opcodes.wb_w;
				  }
				}
orandopt:
| /* */			{ None }
| K_OR			{ Some false }
| K_AND			{ Some true }
cond:
| cond1			{ $1 }
| PAREN_O cond2 PAREN_C	{ $2 }
| PAREN_O cond1 PAREN_C	{ $2 }
cond2:
| cond1 K_AND cond1	{
			  let (aNeg,aZero,aSmall,aBig) = $1 in
			  let (bNeg,bZero,bSmall,bBig) = $3 in
			  ((aNeg && bNeg), (aZero && bZero), (aSmall && bSmall), (aBig && bBig))
			}
| cond1 K_OR cond1	{
			  let (aNeg,aZero,aSmall,aBig) = $1 in
			  let (bNeg,bZero,bSmall,bBig) = $3 in
			  ((aNeg || bNeg), (aZero || bZero), (aSmall || bSmall), (aBig || bBig))
			}
cond1:
| GE NUM		{
			  if $2 = 0.0
                            then (false,true,true,true)
			  else if $2 = 1.0
			    then (false,false,false,true)
			  else
			    failwith(sprintf ">= comparisons supported only with 0 and 1, not %f" $2)
			}
| LT NUM		{ 
			  if $2 = 0.0
                            then (true,false,false,false)
			  else if $2 = 1.0
			    then (true,true,true,false)
			  else
			    failwith(sprintf "< comparisons supported only with 0 and 1, not %f" $2)
			}
| LE NUM		{ 
			  if $2 = 0.0
                            then (true,true,false,false)
			  else
			    failwith(sprintf "<= comparisons supported only with 0, not %f" $2)
			}
| GT NUM		{ 
			  if $2 = 0.0
                            then (false,false,true,true)
			  else
			    failwith(sprintf "> comparisons supported only with 0, not %f" $2)
			}
| EQ NUM		{ 
			  if $2 = 0.0
                            then (false,true,false,false)
			  else
			    failwith(sprintf "= comparisons supported only with 0, not %f" $2)
			}
| NE NUM		{ 
			  if $2 = 0.0
                            then (true,false,true,true)
			  else
			    failwith(sprintf "!= comparisons supported only with 0, not %f" $2)
			}
arg_list:
| arg			{ $1::[] }
| arg COMMA arg_list	{ $1::$3 }
arg:
| opt_minus opt_mult ID	{ Reg($3,None,($1 *. $2)) }
| opt_minus opt_mult ID DOT ID	{ Reg($3,Some $5,($1 *. $2)) }
| opt_minus NUM		{ Imm($1 *. $2) }
opt_minus:
| /* */			{  1.0 }
| MINUS			{ -1.0 }
opt_mult:
| /* */ 		{ 1.0 }
| NUM STAR		{ match $1 with
			  | 0.5 -> 0.5
			  | 1.0 -> 1.0
			  | 2.0 -> 2.0
			  | 4.0 -> 4.0
			  | x   -> failwith(sprintf "The only valid source multipliers are 0.5, 1.0, 2.0 and 4.0, not %f" x)
			}
