open Ast
open Hashtbl

type ty =
  | Void
  | Int
  | Bool
  | String
  | NilPtr
  | Named of string
  | Function of ty list * ty

let ty_is_assignable ty1 ty2 = match (ty1, ty2) with
  | (Void, Void) -> true
  | (Int, Int) -> true
  | (Bool, Bool) -> true
  | (String, String) -> true
  | (Named left, Named right) -> left == right
  | (Named _, NilPtr) | (NilPtr, Named _) -> true
  | _ -> false

let ty_is_struct = function
  | Named(_) -> true
  | NilPtr -> true
  | _ -> false

let ty_is_void = function
  | Void -> true
  | _ -> false

let ty_is_function = function
  | Function(_, _) -> true
  | _ -> false

let ty_is_nil = function
  | NilPtr -> true
  | _ -> false

let ty_name_of = function
  | Named name -> name
  | _ -> failwith "ty_name_of called on unnamed type"

type typed_top_level_decl =
  | TypedFnDecl of string * typed_parameter list * ty * typed_body
  | TypedExternFnDecl of string * typed_parameter list * ty
  | TypedStructDecl of string * typed_field list
and typed_body = typed_statement list
and typed_parameter = string * ty
and typed_field = string * ty
and typed_statement =
  | TypedDeclaration of string * typed_expr
  | TypedWhile of typed_expr * typed_statement list
  | TypedIf of typed_expr * typed_statement list * typed_statement list option
  | TypedReturn of typed_expr option
  | TypedExpression of typed_expr
and typed_expr =
  | TypedLiteral of lit * ty
  | TypedIdentifier of string * ty
  | TypedFieldAccess of typed_expr * string * ty
  | TypedFunctionCall of typed_expr * typed_expr list * ty
  | TypedAssignment of typed_expr * typed_expr * ty
  | TypedArithmeticOp of typed_expr * arith_op * typed_expr * ty
  | TypedLogicalOp of typed_expr * logical_op * typed_expr * ty
  | TypedEqualityOp of typed_expr * equality_op * typed_expr * ty
  | TypedComparisonOp of typed_expr * comparison_op * typed_expr * ty
  | TypedUnOp of unop * typed_expr * ty
  | TypedNew of ty
and arith_op =
  | Add
  | Subtract
  | Multiply
  | Divide
and logical_op =
  | LogicalAnd
  | LogicalOr
and equality_op =
  | Equal
  | NotEqual
and comparison_op =
  | GreaterThanEqual
  | LessThanEqual
  | GreaterThan
  | LessThan

let type_of = function
  | TypedLiteral(_, ty) -> ty
  | TypedIdentifier(_, ty) -> ty
  | TypedFieldAccess(_, _, ty) -> ty
  | TypedFunctionCall(_, _, ty) -> ty
  | TypedAssignment(_, _, ty) -> ty
  | TypedArithmeticOp(_, _, _, ty) -> ty
  | TypedLogicalOp(_, _, _, ty) -> ty
  | TypedEqualityOp(_, _, _, ty) -> ty
  | TypedComparisonOp(_, _, _, ty) -> ty
  | TypedUnOp(_, _, ty) -> ty
  | TypedNew ty -> ty

type typed_compilation_unit = typed_top_level_decl list

type struct_ty_entry = string * typed_field list
type sema_state = {
  mutable symbols: (string, ty) Hashtbl.t list;
  types: (string, struct_ty_entry) Hashtbl.t;
  mutable current_function_return_ty: ty;
  mutable current_function_name: string
}

exception TypeError of string * string

let type_error sym_tab message =
  raise (TypeError (sym_tab.current_function_name, message))

let symtab_query_symbol sym_tab sym =
  let rec chained_find = function
    | [] -> raise Not_found
    | head :: tail -> try Hashtbl.find head sym with
      | Not_found -> chained_find tail in
  try chained_find sym_tab.symbols with
  | Not_found -> type_error sym_tab ("unbound symbol: " ^ sym)

let symtab_query_type sym_tab ty_sym =
  try Hashtbl.find sym_tab.types ty_sym with
  | Not_found -> type_error sym_tab ("unbound type: " ^ ty_sym)

let symtab_query_struct_fields sym_tab struct_ty =
  let struct_name = match struct_ty with
    | Named ty -> ty
      (* field access on anything not a named type is a type error *)
    | _ -> type_error sym_tab "type does not have fields" in
  let (name, fields) = symtab_query_type sym_tab struct_name in fields

let symtab_add_scope sym_tab =
  sym_tab.symbols <- Hashtbl.create 20 :: sym_tab.symbols

let symtab_remove_scope sym_tab =
  sym_tab.symbols <- List.tl sym_tab.symbols

let symtab_add_binding sym_tab key value =
  Hashtbl.add (List.hd sym_tab.symbols) key value

let symtab_remove_binding sym_tab key =
  Hashtbl.remove (List.hd sym_tab.symbols) key

let symtab_add_type_binding sym_tab key value =
  Hashtbl.add sym_tab.types key value

let symtab_remove_type_binding sym_tab key =
  Hashtbl.remove sym_tab.types key

let symtab_create () =
  let symtab = {
    symbols = [Hashtbl.create 20];
    types = Hashtbl.create 20;
    current_function_return_ty = Void;
    current_function_name = "<top level>";
  } in

  symtab_add_type_binding symtab "int" ("int", []);
  symtab_add_type_binding symtab "void" ("void", []);
  symtab_add_type_binding symtab "bool" ("bool", []);
  symtab

let sema_check_assignable sym_tab ty1 ty2 =
  if ty_is_assignable ty1 ty2 then () else type_error sym_tab "types are not assignable"

let sema_lower_type sym_tab ty = match ty with
  | "int" -> Int
  | "void" -> Void
  | "bool" -> Bool
  | "string" -> String
  | name -> let (name, _) = symtab_query_type sym_tab name in Named(name)

let sema_lower_return_type sym_tab = function
  (* not specifying a type defaults to void *)
  | Some named_ty -> sema_lower_type sym_tab named_ty
  | None -> Void

let sema_lower_params sym_tab params =
  let rec _lower_individual = function
    | [] -> []
    | (name, astty) :: tail -> (name, sema_lower_type sym_tab astty) :: _lower_individual tail in
  _lower_individual params

(* expression lowering *)
let rec sema_lower_expression sym_tab expr = match expr with
  | Literal(lit) -> sema_lower_literal sym_tab lit
  | Identifier(ident) -> sema_lower_identifier sym_tab ident
  | FieldAccess(base, field) -> sema_lower_field_access sym_tab base field
  | FunctionCall(base, args) -> sema_lower_function_call sym_tab base args
  | Assignment(target, value) -> sema_lower_assignment sym_tab target value
  | BinOp(lhs, op, rhs) -> sema_lower_binop sym_tab lhs op rhs
  | UnOp(op, target) -> sema_lower_unop sym_tab op target
  | New(ast_ty) -> sema_lower_new sym_tab ast_ty

and sema_lower_literal sym_tab lit = match lit with
  | Bool(b) -> TypedLiteral(Bool(b), Bool)
  | Int(i) -> TypedLiteral(Int(i), Int)
  | String(s) -> TypedLiteral(String(s), String)
  | Nil -> TypedLiteral(Nil, NilPtr)

and sema_lower_identifier sym_tab ident =
  let binding = symtab_query_symbol sym_tab ident in
  TypedIdentifier(ident, binding)

and sema_lower_field_access sym_tab base field =
  let lowered_base = sema_lower_expression sym_tab base in
  (* attempting to access a field on a non-struct type is a type error *)
  let struct_fields = symtab_query_struct_fields sym_tab (type_of lowered_base) in
  (* attempting to access a non-existent field on a non-struct type is a type error *)
  let (field_name, field_ty) = try List.find (fun (name, _) -> name = field) struct_fields with
    | Not_found -> type_error sym_tab "type does not have field" in
  TypedFieldAccess(lowered_base, field_name, field_ty)

and sema_lower_function_call sym_tab base args =
  let lowered_base = sema_lower_expression sym_tab base in
  let lowered_args = List.map (sema_lower_expression sym_tab) args in
  (* lowered_base should be a function type. after asserting that it is
     match the parameter types *)
  let (argument_types, return_type) = match type_of lowered_base with
    | Function(args, retty) -> (args, retty)
    | _ -> type_error sym_tab "expression is not a function" in
  (* check that all parameter types are correct *)
  List.iter2 (sema_check_assignable sym_tab) (List.map type_of lowered_args) argument_types;
  (* the type of the function call is the return type of the function *)
  TypedFunctionCall(lowered_base, lowered_args, return_type)

and sema_lower_assignment sym_tab target value =
  let expr_is_lvalue = function
    | TypedFieldAccess(_, _, _) -> true
    | TypedIdentifier(_, _) -> true
    | _ -> false in
  let lowered_lhs = sema_lower_expression sym_tab target in
  let lowered_rhs = sema_lower_expression sym_tab value in
  (* lhs must be an lvalue. *)
  if not (expr_is_lvalue lowered_lhs) then type_error sym_tab "expression is not an lvalue";
  sema_check_assignable sym_tab (type_of lowered_lhs) (type_of lowered_rhs);
  TypedAssignment(lowered_lhs, lowered_rhs, type_of lowered_rhs)


and sema_lower_binop sym_tab lhs op rhs = match op with
  | Plus | Minus | Div | Mul -> sema_lower_arithmetic_op sym_tab lhs op rhs
  | And | Or -> sema_lower_logical_op sym_tab lhs op rhs
  | Eq | NotEq -> sema_lower_equality_op sym_tab lhs op rhs
  | GreaterEq | LessEq | Greater | Less -> sema_lower_comparison_op sym_tab lhs op rhs

and sema_lower_arithmetic_op sym_tab lhs op rhs =
  let lowered_lhs = sema_lower_expression sym_tab lhs in
  let lowered_rhs = sema_lower_expression sym_tab rhs in
  (* currently, only integers are subject to arithmetic. *)
  sema_check_assignable sym_tab (type_of lowered_lhs) Int;
  sema_check_assignable sym_tab (type_of lowered_rhs) Int;
  match op with
  | Plus -> TypedArithmeticOp(lowered_lhs, Add, lowered_rhs, Int)
  | Minus -> TypedArithmeticOp(lowered_lhs, Subtract, lowered_rhs, Int)
  | Div -> TypedArithmeticOp(lowered_lhs, Divide, lowered_rhs, Int)
  | Mul -> TypedArithmeticOp(lowered_lhs, Multiply, lowered_rhs, Int)
  | _ -> failwith "unreachable: unexpected arithmetic operator"

and sema_lower_logical_op sym_tab lhs op rhs =
  let lowered_lhs = sema_lower_expression sym_tab lhs in
  let lowered_rhs = sema_lower_expression sym_tab rhs in
  sema_check_assignable sym_tab (type_of lowered_lhs) Bool;
  sema_check_assignable sym_tab (type_of lowered_rhs) Bool;
  match op with
  | And -> TypedLogicalOp(lowered_lhs, LogicalAnd, lowered_rhs, Bool)
  | Or -> TypedLogicalOp(lowered_lhs, LogicalOr, lowered_rhs, Bool)
  | _ -> failwith "unreachable: unexpected logical operator"

and sema_lower_equality_op sym_tab lhs op rhs =
  let lowered_lhs = sema_lower_expression sym_tab lhs in
  let lowered_rhs = sema_lower_expression sym_tab rhs in
  sema_check_assignable sym_tab (type_of lowered_lhs) (type_of lowered_rhs);
  match op with
  | Eq -> TypedEqualityOp(lowered_lhs, Equal, lowered_rhs, Bool)
  | NotEq -> TypedEqualityOp(lowered_lhs, NotEqual, lowered_rhs, Bool)
  | _ -> failwith "unreachable: unexpected equality op"

and sema_lower_comparison_op sym_tab lhs op rhs =
  let lowered_lhs = sema_lower_expression sym_tab lhs in
  let lowered_rhs = sema_lower_expression sym_tab rhs in
  (* for now, only integers can be compared *)
  sema_check_assignable sym_tab (type_of lowered_lhs) Int;
  sema_check_assignable sym_tab (type_of lowered_rhs) Int;
  match op with
  | GreaterEq -> TypedComparisonOp(lowered_lhs, GreaterThan, lowered_rhs, Bool)
  | LessEq -> TypedComparisonOp(lowered_lhs, LessThan, lowered_rhs, Bool)
  | Greater -> TypedComparisonOp(lowered_lhs, GreaterThan, lowered_rhs, Bool)
  | Less -> TypedComparisonOp(lowered_lhs, LessThan, lowered_rhs, Bool)
  | _ -> failwith "unreachable: unexpected comparison op"

and sema_lower_unop sym_tab op target =
  let lowered_target = sema_lower_expression sym_tab target in
  match op with
  | Not -> begin
      sema_check_assignable sym_tab (type_of lowered_target) Bool;
      TypedUnOp(Not, lowered_target, Bool)
    end
  | UnaryMinus -> begin
      sema_check_assignable sym_tab (type_of lowered_target) Int;
      TypedUnOp(UnaryMinus, lowered_target, Int)
    end

and sema_lower_new sym_tab ty =
  let lowered_ty = sema_lower_type sym_tab ty in
  if not (ty_is_struct lowered_ty) then type_error sym_tab "only structs can be created with operator new";
  TypedNew(lowered_ty)

(* statement lowering *)
let rec sema_lower_declaration sym_tab name binding =
  (* a decl like "let x = 42" automatically uses the type of
     the binding expression to infer the type of "x" *)
  let lowered_binding = sema_lower_expression sym_tab binding in
  let binding_ty = type_of lowered_binding in
  symtab_add_binding sym_tab name binding_ty;
  TypedDeclaration(name, lowered_binding)

and sema_lower_while sym_tab cond body =
  let lowered_cond = sema_lower_expression sym_tab cond in
  sema_check_assignable sym_tab (type_of lowered_cond) Bool;
  symtab_add_scope sym_tab;
  let lowered_body = sema_lower_statement_list sym_tab body in
  symtab_remove_scope sym_tab;
  TypedWhile(lowered_cond, lowered_body)

and sema_lower_if sym_tab cond true_branch false_branch =
  let lowered_cond = sema_lower_expression sym_tab cond in
  sema_check_assignable sym_tab (type_of lowered_cond) Bool;
  (* each arm of the if statement is a new scope *)
  let lowered_true = begin
    symtab_add_scope sym_tab;
    let result = sema_lower_statement_list sym_tab true_branch in
    symtab_remove_scope sym_tab;
    result
  end in
  let lowered_false = begin
    symtab_add_scope sym_tab;
    let result = match false_branch with
    | Some branch -> Some(sema_lower_statement_list sym_tab branch)
    | None -> None in
    symtab_remove_scope sym_tab;
    result
  end in
  TypedIf(lowered_cond, lowered_true, lowered_false)

and sema_lower_return sym_tab expr =
  let lowered_expr = match expr with
    | Some ret_expr -> begin
        (* if this function returns void, any expression is a type error *)
        if sym_tab.current_function_return_ty = Void then type_error sym_tab "can't return expression in void function";
        let lowered = sema_lower_expression sym_tab ret_expr in
        (* otherwise the returned type must agree with the function's return type *)
        sema_check_assignable sym_tab (type_of lowered) sym_tab.current_function_return_ty;
        Some(lowered)
      end
    | None -> begin
        (* if this function does not return void, this is a type error *)
           if sym_tab.current_function_return_ty <> Void then type_error sym_tab "bare return in non-void function";
        None
      end in
  TypedReturn(lowered_expr)

and sema_lower_statement sym_tab stmt = match stmt with
  | Declaration(name, binding) -> sema_lower_declaration sym_tab name binding
  | While(cond, body) -> sema_lower_while sym_tab cond body
  | If(cond, true_branch, false_branch) -> sema_lower_if sym_tab cond true_branch false_branch
  | Return(expr) -> sema_lower_return sym_tab expr
  | Expression(expr) -> TypedExpression(sema_lower_expression sym_tab expr)

and sema_lower_statement_list sym_tab body =
  List.map (sema_lower_statement sym_tab) body

(* top-level declaration lowering *)
let sema_lower_fn_decl sym_tab name params retty body =
  let lowered_params = sema_lower_params sym_tab params in
  let lowered_type = sema_lower_return_type sym_tab retty in
  (* we have to insert this function into the current environment
     with the signature we just verified *)
  let lowered_param_types = List.map (fun (_, ty) -> ty) lowered_params in
  let fun_ty = Function(lowered_param_types, lowered_type) in
  (* enter a new scope - the scope of the function *)
  symtab_add_binding sym_tab name fun_ty;

  (* lower the parameters into the current environment - we
     are about to type check the body of the function. *)
  symtab_add_scope sym_tab;
  List.iter (fun (key, ty) -> symtab_add_binding sym_tab key ty) lowered_params;
  let last_return_ty = sym_tab.current_function_return_ty in
  let last_current_fn = sym_tab.current_function_name in
  sym_tab.current_function_return_ty <- lowered_type;
  sym_tab.current_function_name <- name;
  let lowered_body = sema_lower_statement_list sym_tab body in
  symtab_remove_scope sym_tab;
  sym_tab.current_function_return_ty <- last_return_ty;
  sym_tab.current_function_name <- last_current_fn;
  TypedFnDecl(name, lowered_params, lowered_type, lowered_body)


let sema_lower_extern_fn_decl sym_tab name params retty =
  (* nothing to do here except validate the types and insert
     the function into the current environment *)
  let lowered_params = sema_lower_params sym_tab params in
  let lowered_type = sema_lower_return_type sym_tab retty in
  let lowered_param_types = List.map (fun (_, ty) -> ty) lowered_params in
  let fun_ty = Function(lowered_param_types, lowered_type) in
  symtab_add_binding sym_tab name fun_ty;
  TypedExternFnDecl(name, lowered_params, lowered_type)

(* currently, fields and function parameters are represented the same way. *)
let sema_lower_fields = sema_lower_params

let sema_lower_struct_decl sym_tab name fields =
  let rec duplicate_check = function
    | [] -> false
    | head :: tail -> if List.exists (fun t -> t = head) tail then true else duplicate_check tail in
  (* we first have to insert a partial copy of this struct into the type table so that
     recursive structs are possible *)
  symtab_add_type_binding sym_tab name (name, []);
  let lowered_fields = sema_lower_fields sym_tab fields in
  (* remove the partial copy *)
  symtab_remove_type_binding sym_tab name;
  (* checks for fields. right now the only check is to ensure
     that every field has a unique name *)
  if duplicate_check fields then type_error sym_tab "duplicate fields in struct";
  (* add an entry into the type symbol table *)
  symtab_add_type_binding sym_tab name (name, lowered_fields);
  TypedStructDecl(name, lowered_fields)

let sema_lower_top_level_decl sym_tab decl =
  match decl with
  | FnDecl(name, params, retty, body) -> sema_lower_fn_decl sym_tab name params retty body
  | ExternFnDecl(name, params, retty) -> sema_lower_extern_fn_decl sym_tab name params retty
  | StructDecl(name, fields)          -> sema_lower_struct_decl sym_tab name fields

let sema_lower_compilation_unit unit =
  let symtab = symtab_create() in
  List.map (sema_lower_top_level_decl symtab) unit
