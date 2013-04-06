let fn = if Array.length Sys.argv < 2
         then failwith "Usage: slcompiler <filename.sl>"
	 else Sys.argv.(1)
let stream_in  = open_in fn
let ast = (Parser.shaderfile Lexer.token (Lexing.from_channel stream_in));;
let ast2 = Compile.compile ast
let ast2_s = Print_ast2.ast2_to_string ast2
let _ = Printf.printf "%s" ast2_s
(*
let code = Codegen.codegen ast2
let _ = Printf.printf "\nGenerated code:\n%s" code
*)
