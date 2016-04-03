%{

open Ast

%}

%token <string> IDENT
%token <int> INT
%token <string> STRING
%token FN LET IF ELSE WHILE STRUCT TRUE FALSE EXTERN RETURN NEW NIL
%token LPAREN RPAREN LBRACKET RBRACKET
%token ARROW COMMA SEMICOLON COLON DOT
%token EQUAL DOUBLE_EQ NOT_EQ GREATER_EQ LESS_EQ GREATER LESS
%token PLUS MINUS DIV MUL
%token AND OR NOT
%token EOF

%start compilation_unit
%type <Ast.compilation_unit> compilation_unit

%%

compilation_unit:
     | top_level_decl EOF { [$1] }
     | top_level_decl compilation_unit { $1 :: $2 }

top_level_decl:
     | FN IDENT LPAREN parameters RPAREN return_type LBRACKET statement_list RBRACKET
       { FnDecl($2, $4, $6, $8) }
     | EXTERN FN IDENT LPAREN parameters RPAREN return_type SEMICOLON
       { ExternFnDecl($3, $5, $7) } 
     | STRUCT IDENT LBRACKET fields RBRACKET
       { StructDecl($2, $4) }

parameters:
     | { [] }
     | nonempty_parameters { $1 }

nonempty_parameters:
     | IDENT COLON ty { [($1, $3)] }
     | IDENT COLON ty COMMA nonempty_parameters { ($1, $3) :: $5 }

return_type:
     | { None }
     | ARROW ty { Some($2) }

ty:
     | IDENT { $1 }

fields:
     | IDENT COLON ty { [($1, $3)] }
     | IDENT COLON ty COMMA fields { ($1, $3) :: $5}

statement_list:
     | { [] }
     | nonempty_statement_list { $1 }

nonempty_statement_list:
     | statement { [$1] }
     | statement nonempty_statement_list { $1 :: $2 }

statement:
     | LET IDENT EQUAL expression SEMICOLON
      { Declaration($2, $4) }
     | WHILE LPAREN expression RPAREN LBRACKET statement_list RBRACKET
      { While($3, $6) }
     | IF LPAREN expression RPAREN LBRACKET statement_list RBRACKET
      { If($3, $6, None) }
     | IF LPAREN expression RPAREN LBRACKET statement_list RBRACKET ELSE LBRACKET statement_list RBRACKET
      { If($3, $6, Some($10)) }
     | RETURN SEMICOLON
      { Return(None) }
     | RETURN expression SEMICOLON
      { Return(Some($2)) }
     | expression SEMICOLON
      { Expression($1) }

expression:
     | expression EQUAL or_expression
       { Assignment($1, $3) }
     | or_expression
       { $1 }

or_expression:
     | or_expression OR and_expression
      { BinOp($1, Or, $3) }
     | and_expression
      { $1 }

and_expression:
     | and_expression AND equality_expression
      { BinOp($1, And, $3) }
     | equality_expression
      { $1 }

equality_expression:
     | equality_expression DOUBLE_EQ comparison_expression
      { BinOp($1, Eq, $3) }
     | equality_expression NOT_EQ comparison_expression
      { BinOp($1, NotEq, $3) }
     | comparison_expression
      { $1 }

comparison_expression:
     | comparison_expression GREATER arithmetic_expression
      { BinOp($1, Greater, $3) }
     | comparison_expression LESS arithmetic_expression
      { BinOp($1, Less, $3) }
     | comparison_expression GREATER_EQ arithmetic_expression
      { BinOp($1, GreaterEq, $3) }
     | comparison_expression LESS_EQ arithmetic_expression
      { BinOp($1, LessEq, $3) }
     | arithmetic_expression
      { $1 }

arithmetic_expression:
     | arithmetic_expression PLUS mul_expression
      { BinOp($1, Plus, $3) }
     | arithmetic_expression MINUS mul_expression
      { BinOp($1, Minus, $3) }
     | mul_expression
      { $1 }

mul_expression:
     | mul_expression MUL unary_expression
      { BinOp($1, Mul, $3) }
     | mul_expression DIV unary_expression
      { BinOp($1, Div, $3) }
     | unary_expression
      { $1 }
unary_expression:
     | MINUS unary_expression
      { UnOp(UnaryMinus, $2) }
     | NOT unary_expression
      { UnOp(Not, $2) }
     | call_expression
      { $1 }

call_expression:
     | call_expression LPAREN comma_sep_expr RPAREN
      { FunctionCall($1, $3) }
     | member_expression
      { $1 }

member_expression:
     | member_expression DOT IDENT
      { FieldAccess($1, $3) }
     | primary_expression
      { $1 }

primary_expression:
     | IDENT { Identifier($1) }
     | INT { Literal(Int($1)) }
     | TRUE { Literal(Bool(true)) }
     | FALSE { Literal(Bool(false)) }
     | STRING { Literal(String($1)) }
     | NIL { Literal(Nil) }
     | NEW ty { New($2) }

comma_sep_expr:
     | { [] }
     | nonempty_comma_sep_expr { $1 }

nonempty_comma_sep_expr:
     | expression { [$1] }
     | expression COMMA nonempty_comma_sep_expr { $1 :: $3 }
