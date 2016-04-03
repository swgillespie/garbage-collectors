let parse_with_error input_file_name lexbuf =
  let ast = Parser.compilation_unit Lexer.token lexbuf in
  let typed_ast = Sema.sema_lower_compilation_unit ast in
  let llmodule = Trans.trans_lower_compilation_unit input_file_name typed_ast in
  Llvm_analysis.assert_valid_module llmodule;
  let output = (Filename.chop_extension input_file_name) ^ ".ll" in
  Llvm.print_module output llmodule;;


let main () =
  if Array.length Sys.argv <> 2 then begin
    print_string "error: no input files\n";
    exit 1;
  end;
  let buf = Llvm.MemoryBuffer.of_file Sys.argv.(1) in
  let string = Llvm.MemoryBuffer.as_string buf in
  let lexbuf = Lexing.from_string string in
  try parse_with_error Sys.argv.(1) lexbuf with
  | Parsing.Parse_error -> print_string "fatal: parse error\n"; exit 1
  | Failure(msg) -> Printf.printf "fatal error: %s\n" msg; exit 1
  | Sema.TypeError(func, msg) -> Printf.printf "fatal: in function \"%s\": %s\n" func msg; exit 1;;

main()
