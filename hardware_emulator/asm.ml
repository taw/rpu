open Printf

(* FIXME: this is an ugly hack.
 *
 * Real secondaries depend on condition of the primary instruction.
 * Fake secondaries do not.
 *)
type sec = Real_secondary of Opcodes.cond * ((string, int) Hashtbl.t -> Opcodes.op_conditional)
         | Fake_secondary of ((string, int) Hashtbl.t -> Opcodes.op)

let convert_sec : Asm_ast.stmt_secondary -> sec
= function
| `LOAD(i,a,n)     -> Fake_secondary (fun _ -> `LOAD(i,a,n))
| `LOAD4(a,n)      -> Fake_secondary (fun _ -> `LOAD4(a,n))
| `TRACE(s1,s2,s3) -> Fake_secondary (fun _ -> `TRACE(s1,s2,s3))
| `CJMP(c,lab)     -> Real_secondary (
                           c, fun labels -> (`JMP(try Hashtbl.find labels lab
					     with _ -> failwith (sprintf "Label %s not found" lab))
                      ))
| `CALL(lab,psz)   -> Fake_secondary (fun labels -> `CALL((try Hashtbl.find labels lab with _ -> failwith (sprintf "Label %s not found" lab)),psz))
| `DCALL(psz)      -> Fake_secondary (fun _ -> `DCALL(psz))
| `JMP(lab)        -> Fake_secondary (fun labels -> `JMP(try Hashtbl.find labels lab with _ -> failwith (sprintf "Label %s not found" lab)))
| `RETURN(c)       -> Real_secondary (c, fun _ -> `RETURN)
| `EXIT(c)         -> failwith "EXIT opcode not supported"

let (convert_shader : Asm_ast.stmt list->(Opcodes.op array * (string, int) Hashtbl.t))
= fun ast ->
    let cur_instr = ref 0 in
    let shader_out : ((((string,int) Hashtbl.t) -> Opcodes.op) list) ref = ref [] in
    let labels = Hashtbl.create 0 in
    let register_label lab   = if Hashtbl.mem labels lab
			       then failwith "Label %s defined more than once"
			       else Hashtbl.replace labels lab (!cur_instr)
in
    let register_instr instr = (shader_out := (instr::!shader_out); incr cur_instr) in
    let rec convert_stmt = function
    | #Asm_ast.stmt_arith as i     -> let i = (i :> Opcodes.op) in
                                      register_instr (fun _ -> i)
    | #Asm_ast.stmt_complex as i   -> let i = (i :> Opcodes.op) in
                                      register_instr (fun _ -> i)
    | #Asm_ast.stmt_secondary as i -> (match convert_sec i with
				      | Real_secondary _ -> failwith "Lone real secondaries not supported yet"
				      | Fake_secondary i -> register_instr i
				      ) 
    | #Asm_ast.stmt_single    -> failwith "Single mode instructions (loads/stores/texture) not supported"
    | `PAIR((#Asm_ast.stmt_arith),(#Asm_ast.stmt_arith)) -> failwith "Arith/arith splitting not supported"
    | `PAIR((#Asm_ast.stmt_complex),(#Asm_ast.stmt_arith)) -> failwith "Complex/arith splitting not supported"
    | `PAIR((#Asm_ast.stmt_arith as i1),(#Asm_ast.stmt_secondary as i2))
                             -> let i1 = (i1 :> Opcodes.op_arith) in
			        let i2 = convert_sec i2 in
				(match i2 with
				| Real_secondary (c,i2) -> register_instr (fun labels -> `COND(i1, c, i2 labels))
				| Fake_secondary _ -> failwith "This instr/secondary pairing is not supported yet"
				)
    | `PAIR((#Asm_ast.stmt_complex as i1),(#Asm_ast.stmt_secondary as i2))
                             -> let i1 = (i1 :> Opcodes.op_arith) in
			        let i2 = convert_sec i2 in
				(match i2 with
				| Real_secondary (c,i2) -> register_instr (fun labels -> `COND(i1, c, i2 labels))
				| Fake_secondary _  -> failwith "This instr/secondary pairing is not supported yet"
				)
    | `LABEL(lab)  -> register_label lab
    | `PAR(instr)-> List.iter (convert_stmt) instr
in
    let _ = List.iter convert_stmt ast in
(* Extra RETURN just in case the shader does not return 
 * (a crash is still possible in case of incorrect jump)
 *)
    let rv = (Array.of_list (List.map (function x -> x labels) (List.rev ((fun _->`RETURN)::!shader_out)))) in
    (rv, labels)
    

let print_entry_points labels =
    Hashtbl.iter (fun k v ->
        fprintf stderr "%s %d\n" k v
    ) labels

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

let ast = Asm_parser.program Asm_lexer.token (Lexing.from_channel chan_in)
let _ = if !print_ast_only
	then Asm_ast.print_ast ast
	else let (shader, labels)   = convert_shader ast in
	     let chan_out = (match !(output_file) with None -> stdout | Some x -> open_out x) in
             (
	     output_value chan_out shader;
             print_entry_points labels
             )
