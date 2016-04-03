SOURCES = lexer.mll parser.mly ast.ml sema.ml trans.ml main.ml
RESULT = shortyc

CLIBS = llvm LLVMCore LLVMSupport
LIBS = llvm llvm_analysis llvm_executionengine llvm_target llvm_scalar_opts unix

OCAMLOPT = ocamlopt -cc g++ -I /usr/local/lib/ocaml -I /usr/local/lib/ocaml 
OCAMLC = ocamlc -I /usr/local/lib/ocaml


include OCamlMakefile
