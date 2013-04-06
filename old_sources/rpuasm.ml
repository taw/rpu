open Printf

(*
 * Ast -> Shader conversion
 *)
let convert_sw = function
    | Ast.X -> 0
    | Ast.Y -> 1
    | Ast.Z -> 2
    | Ast.W -> 3
let convert_smult = function
    | (Ast.SM_HALF) -> 0.5
    | (Ast.SM_1)    -> 1.0
    | (Ast.SM_2)    -> 2.0
    | (Ast.SM_4)    -> 4.0
let convert_smod (neg,smult) = (if neg then -1.0 else 1.0) *. (convert_smult smult)
let convert_reg = function
    | "R0"  -> 0
    | "R1"  -> 1
    | "R2"  -> 2
    | "R3"  -> 3
    | "R4"  -> 4
    | "R5"  -> 5
    | "R6"  -> 6
    | "R7"  -> 7
    | "R8"  -> 8
    | "R9"  -> 9
    | "R10" -> 10
    | "R11" -> 11
    | "R12" -> 12
    | "R13" -> 13
    | "R14" -> 14
    | "R15" -> 15
    | "S"   -> 16
    | r     -> failwith (sprintf "Unknown register %s" r)

let convert_target (r, wbm) = (convert_reg r, wbm)
let convert_src = function
    | Ast.SrcImm x          -> Shader.SrcImm x
    | Ast.SrcReg(r,(xsw,ysm,zsw,wsw),smod) -> Shader.SrcReg(convert_reg r,
		 (convert_sw xsw,convert_sw ysm,convert_sw zsw,convert_sw wsw),convert_smod smod)

let convert_imod = function
    | None -> None
    | Some Ast.SAT -> Some Shader.SAT
    | Some Ast.RCP -> Some Shader.RCP
    | Some Ast.RSQ -> Some Shader.RSQ

let convert_arith = function
| `MOV (imod,target,s1)       -> (Shader.MOV, convert_imod imod,(convert_target target),[convert_src s1])
| `FRAC(imod,target,s1)       -> (Shader.FRAC,convert_imod imod,(convert_target target),[convert_src s1])
| `ADD (imod,target,s1,s2)    -> (Shader.ADD, convert_imod imod,(convert_target target),[convert_src s1;convert_src s2])
| `MUL (imod,target,s1,s2)    -> (Shader.MUL, convert_imod imod,(convert_target target),[convert_src s1;convert_src s2])
| `MAD (imod,target,s1,s2,s3) -> (Shader.MAD, convert_imod imod,(convert_target target),[convert_src s1;convert_src s2;convert_src s3])

let convert_complex = function
| `DP3 (imod,target,s1,s2) -> (Shader.DP3, convert_imod imod,(convert_target target),[convert_src s1;convert_src s2])
| `DP2H(imod,target,s1,s2) -> (Shader.DP2H,convert_imod imod,(convert_target target),[convert_src s1;convert_src s2])
| `DP4 (imod,target,s1,s2) -> (Shader.DP4, convert_imod imod,(convert_target target),[convert_src s1;convert_src s2])
| `DP3H(imod,target,s1,s2) -> (Shader.DP3H,convert_imod imod,(convert_target target),[convert_src s1;convert_src s2])

let convert_red = function
    | Ast.RED_OR -> Shader.RED_OR
    | Ast.RED_AND-> Shader.RED_AND
let convert_cond (c,m,r) = (c,m,convert_red r)

let convert_sec = function
| `LOAD(_,_,_) -> failwith "LOAD not supported yet"
| `LOAD4(_,_)  -> failwith "LOAD4 not supported yet"
| `CJMP(c,lab) -> fun labels -> Shader.CJMP(convert_cond c, try Hashtbl.find labels lab
							       with _ -> failwith (sprintf "Label %s not found" lab))
| `CALL(_,_)   -> failwith "CALL not supported yet"
| `TRACE(_,_)  -> failwith "TRACE not supported yet"
| `RETURN(c)   -> fun _ -> Shader.RETURN(convert_cond c)
| `EXIT(_)     -> failwith "EXIT not supported yet"

let (convert_shader : Ast.stmt list->Shader.shader) = fun shader ->
    let cur_instr = ref 0
 in let shader_out = ref []
 in let labels = Hashtbl.create 0
 in let register_label lab   = if Hashtbl.mem labels lab
			       then failwith "Label %s defined more than once"
			       else Hashtbl.replace labels lab (!cur_instr)
 in let register_instr instr = (shader_out := (instr::!shader_out); incr cur_instr)
 in let rec convert_stmt = function
    | #Ast.stmt_arith as i     -> register_instr (convert_arith i, None)
    | #Ast.stmt_complex as i   -> register_instr (convert_complex i, None)
    | #Ast.stmt_secondary as i -> failwith "Lone secondary instructions not supported"
    | #Ast.stmt_single as i    -> failwith "Single mode instructions (loads/stores/texture) not supported"
    | `PAIR((#Ast.stmt_arith as i1),(#Ast.stmt_arith as i2)) -> failwith "Arith/arith splitting not supported"
    | `PAIR((#Ast.stmt_complex as i1),(#Ast.stmt_arith as i2)) -> failwith "Complex/arith splitting not supported"
    | `PAIR((#Ast.stmt_arith as i1),(#Ast.stmt_secondary as i2)) -> register_instr (convert_arith i1, Some (convert_sec i2))
    | `PAIR((#Ast.stmt_complex as i1),(#Ast.stmt_secondary as i2)) -> register_instr (convert_complex i1, Some (convert_sec i2))
    | `LABEL(lab)  -> register_label lab
    | `PAR(instr)-> List.iter (convert_stmt) instr
 in let _ = List.iter convert_stmt shader
 in Array.of_list (List.map (function
    | (a, None)   -> (a, None)
    | (a, Some x) -> (a, Some (x labels))
 )(List.rev !shader_out))

(*********************************************************
 * MAIN                                                  *
 *********************************************************)
let input_file     = ref None
let print_ast_only = ref false
let output_file    = ref None

let arg_spec = [
    ("-i", Arg.String (fun arg -> input_file := Some(arg)), "input file");
    ("--ast", Arg.Unit (fun () -> print_ast_only := true), "print AST only");
    ("-o", Arg.String (fun arg -> output_file := Some(arg)),  "output file");
]
let parse_rest str = failwith (sprintf "Parse error: argument %s not understood" str)

let usage_msg = "./rpuasm -s <source> [-o <target>] [--ast]"
let _ = Arg.parse arg_spec parse_rest usage_msg

let chan_in = match !input_file with
    | None   -> stdin
    | Some x -> open_in x

let ast = Parser.program Lexer.token (Lexing.from_channel chan_in)
let _ = if !print_ast_only
	then Ast.print_ast ast
	else let shader   = convert_shader ast
	  in let chan_out = (match !(output_file) with None -> stdout | Some x -> open_out x)
	  in output_value chan_out shader
