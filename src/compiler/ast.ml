type top_level_decl =
  | FnDecl of string * parameter list * ast_ty option * fn_body
  | ExternFnDecl of string * parameter list * ast_ty option
  | StructDecl of string * field list
and parameter = string * ast_ty
and ast_ty = string
and field = string * ast_ty
and fn_body = statement list
and statement =
  | Declaration of string * expr
  | While of expr * statement list
  | If of expr * statement list * statement list option
  | Return of expr option
  | Expression of expr
and expr =
  | Literal of lit
  | Identifier of string
  | FieldAccess of expr * string
  | FunctionCall of expr * expr list
  | Assignment of expr * expr
  | BinOp of expr * binop * expr
  | UnOp of unop * expr
  | New of ast_ty
and lit =
  | Bool of bool
  | Int of int
  | String of string
  | Nil
and binop =
  | Plus
  | Minus
  | Div
  | Mul
  | And
  | Or
  | Eq
  | NotEq
  | GreaterEq
  | LessEq
  | Greater
  | Less
and unop =
  | Not
  | UnaryMinus

type compilation_unit = top_level_decl list
