{

open Lexing
open Parser

exception SyntaxError

}

let ident = ['_' 'A'-'Z' 'a'-'z']['_' 'A'-'Z' 'a'-'z' '0'-'9']*
let integer = ['0'-'9']+

rule token = parse
            | [' ' '\t' '\n' '\r'] { token lexbuf }
            | "fn" { FN }
            | "let" { LET }
            | "if" { IF }
            | "else" { ELSE }
            | "while" { WHILE }
            | "struct" { STRUCT }
            | "true" { TRUE }
            | "false" { FALSE }
            | "and" { AND }
            | "or" { OR }
            | "not" { NOT }
            | "extern" { EXTERN }
            | "return" { RETURN }
            | "new" { NEW }
            | "nil" { NIL }
            | ident { IDENT (Lexing.lexeme lexbuf)}
            | integer { INT (int_of_string (Lexing.lexeme lexbuf))}
            | '(' { LPAREN }
            | ')' { RPAREN }
            | '{' { LBRACKET }
            | '}' { RBRACKET }
            | '.' { DOT }
            | ';' { SEMICOLON }
            | ':' { COLON }
            | "->" { ARROW }
            | ',' { COMMA }
            | '=' { EQUAL }
            | "==" { DOUBLE_EQ }
            | "!=" { NOT_EQ }
            | ">=" { GREATER_EQ }
            | "<=" { LESS_EQ }
            | ">" { GREATER }
            | "<" { LESS }
            | "+" { PLUS }
            | "-" { MINUS }
            | "/" { DIV }
            | "*" { MUL }
            | eof { EOF }
            | "\"" { read_string (Buffer.create 20) lexbuf }
            | _ { raise SyntaxError }

and read_string buf = parse
                      | '"'       { STRING (Buffer.contents buf) }
                      | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
                      | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
                      | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
                      | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
                      | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
                      | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
                      | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
                      | [^ '"' '\\']+
                        { Buffer.add_string buf (Lexing.lexeme lexbuf);
                          read_string buf lexbuf
                        }
                      | _ { raise SyntaxError }
                      | eof { raise SyntaxError }
