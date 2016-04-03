open Ast
open Sema

type trans_state = {
  struct_field_table: (string, Llvm.lltype * (string * int) list) Hashtbl.t;
  type_info_table: (string, Llvm.llvalue) Hashtbl.t;
  context: Llvm.llcontext;
  llvm_mod: Llvm.llmodule;
  identifier_map: (string, Llvm.llvalue) Hashtbl.t;
  mutable rtti_struct: Llvm.lltype option;
  mutable builder: Llvm.llbuilder;
  mutable current_fn_exit_block: Llvm.llbasicblock option;
  mutable current_fn_return_value: Llvm.llvalue option;
  mutable current_fn_return_ty: Llvm.lltype option;
}

let trans_state_add_struct context llvmty name fields =
  (* assumption - every struct is defined exactly once *)
  let rec list_to_indexes i list = match list with
    | [] -> []
    | (head, _) :: tail -> (head, i) :: list_to_indexes (i + 1) tail in
  let fields_by_index = list_to_indexes 0 fields in
  Hashtbl.add context.struct_field_table name (llvmty, fields_by_index);;

let trans_state_get_struct_offset context ty field =
  let ty_name = match ty with
    | Named name -> name
    | _ -> failwith "invalid type for trans_state_get_struct_offset" in
  let struct_entry = Hashtbl.find context.struct_field_table ty_name in
  let field_table = snd struct_entry in
  List.assoc field field_table

let trans_state_create context llvm_mod builder = {
  struct_field_table = Hashtbl.create 20;
  type_info_table = Hashtbl.create 20;
  context = context;
  llvm_mod = llvm_mod;
  builder = builder;
  identifier_map = Hashtbl.create 20;
  rtti_struct = None;
  current_fn_exit_block = None;
  current_fn_return_value = None;
  current_fn_return_ty = None;
}

let unwrap = function
  | Some v -> v
  | None -> raise Not_found

let rtti_for trans_state ty =
  let rtti = unwrap (Llvm.lookup_global ("__" ^ (ty_name_of ty) ^ "_type_info") trans_state.llvm_mod) in
  Llvm.build_bitcast rtti (Llvm.pointer_type (Llvm.i8_type trans_state.context)) "" trans_state.builder

let rec llvm_type_of trans_state = function
  | Void -> Llvm.i1_type trans_state.context
  | Int -> Llvm.i32_type trans_state.context
  | Bool -> Llvm.i1_type trans_state.context
  | String -> Llvm.qualified_pointer_type (Llvm.i8_type trans_state.context) 0
  | NilPtr -> Llvm.qualified_pointer_type (Llvm.i8_type trans_state.context) 1
  | Named name -> llvm_type_of_struct trans_state name
  | Function(args, ret) -> llvm_type_of_function trans_state args ret


and llvm_type_of_struct trans_state name =
  let table = Hashtbl.find trans_state.struct_field_table name in
  let llty = fst table in
  Llvm.qualified_pointer_type llty 1

and llvm_type_of_function trans_state args ret =
  let llvm_arg_tys = List.map (llvm_type_of trans_state) args in
  let llvm_retty = if ty_is_void ret then Llvm.void_type trans_state.context else llvm_type_of trans_state ret in
  Llvm.function_type llvm_retty (Array.of_list llvm_arg_tys)

let trans_current_block context =
  Llvm.insertion_block context.builder

let trans_current_function context =
  Llvm.block_parent (trans_current_block context)

let rec trans_lower_expression trans_state expr = match expr with
  | TypedLiteral(lit, _) -> trans_lower_literal trans_state lit
  | TypedIdentifier(ident, _) -> trans_lower_identifier trans_state ident
  | TypedFieldAccess(base, field, _) -> trans_lower_field_access trans_state base field
  | TypedFunctionCall(base, args, _) -> trans_lower_function_call trans_state base args
  | TypedAssignment(target, binding, _) -> trans_lower_assignment trans_state target binding
  | TypedArithmeticOp(left, op, right, _) -> trans_lower_arithmetic_op trans_state left op right
  | TypedLogicalOp(left, op, right, _) -> trans_lower_logical_op trans_state left op right
  | TypedEqualityOp(left, op, right, _) -> trans_lower_equality_op trans_state left op right
  | TypedComparisonOp(left, op, right, _) -> trans_lower_comparison_op trans_state left op right
  | TypedUnOp(op, target, _) -> trans_lower_unop trans_state op target
  | TypedNew(ty) -> trans_lower_new trans_state ty

and trans_lower_literal trans_state lit = match lit with
  | Bool true -> Llvm.const_int (llvm_type_of trans_state Bool) 1
  | Bool false -> Llvm.const_int (llvm_type_of trans_state Bool) 0
  | Int i -> Llvm.const_int (llvm_type_of trans_state Int) i
  | String s -> begin
      let global = Llvm.build_global_string s "lit" trans_state.builder in
      Llvm.build_struct_gep global 0 "lit" trans_state.builder
    end
  | Nil -> Llvm.const_pointer_null (llvm_type_of trans_state NilPtr)


and trans_lower_identifier trans_state ident =
  let local = Hashtbl.find trans_state.identifier_map ident in
  Llvm.build_load local "" trans_state.builder

and trans_lower_field_access trans_state base field =
  let struct_base_reg = trans_lower_expression trans_state base in
  let struct_ty = type_of base in
  let offset = trans_state_get_struct_offset trans_state struct_ty field in
  let gep = Llvm.build_struct_gep struct_base_reg offset "" trans_state.builder in
  Llvm.build_load gep "" trans_state.builder

and trans_lower_function_call trans_state base args =
  let lowered_base = match base with
    | TypedIdentifier(ident, _) -> Hashtbl.find trans_state.identifier_map ident
    | expr -> trans_lower_expression trans_state expr in
  let cconv = Llvm.function_call_conv lowered_base in
  let lowered_args = List.map (trans_lower_expression trans_state) args in
  let call = Llvm.build_call lowered_base (Array.of_list lowered_args) "" trans_state.builder in
  Llvm.set_instruction_call_conv cconv call;
  call

and trans_lower_assignment trans_state target binding =
  let binding_alloca = Llvm.build_alloca (llvm_type_of trans_state (type_of target)) "" trans_state.builder in
  let binding_reg = trans_lower_expression trans_state binding in
  let final_binding_reg = if ty_is_nil (type_of binding) then begin
      (* if the RHS is a nil, we need to bitcast to the target pointer type. *)
      Llvm.build_bitcast binding_reg (llvm_type_of trans_state (type_of target)) "" trans_state.builder
    end else binding_reg in
  ignore (Llvm.build_store final_binding_reg binding_alloca trans_state.builder);
  match target with
  | TypedFieldAccess(base, field, _) -> begin
      (* this is a three step process for expressions of the format_of_string
         "thing.field = value".
         First, we lower "thing" as it will be the base of the GEP.
         Next, we calculate the offset of "field" into the struct
         that "thing" returns, which will be the offset of the gep
         Finally, we will emit a store from binding_reg into the gep. *)
      let struct_base_reg = trans_lower_expression trans_state base in
      let struct_ty = type_of base in
      let offset = trans_state_get_struct_offset trans_state struct_ty field in
      let gep = Llvm.build_struct_gep struct_base_reg offset "" trans_state.builder in
      let binding_ret_reg = Llvm.build_load binding_alloca "" trans_state.builder in
      ignore (Llvm.build_store binding_ret_reg gep trans_state.builder);
      binding_ret_reg
    end
  | TypedIdentifier(name, _) -> begin
      (* it's a lot easier if this is an identifier *)
      let binding_ret_reg = Llvm.build_load binding_alloca "" trans_state.builder in
      let ident_alloca = Hashtbl.find trans_state.identifier_map name in
      ignore (Llvm.build_store binding_ret_reg ident_alloca trans_state.builder);
      binding_ret_reg
    end
  | _ -> failwith "invalid lvalue in trans_lower_assignment"

and trans_lower_arithmetic_op trans_state left op right =
  let lowered_left = trans_lower_expression trans_state left in
  let lowered_right = trans_lower_expression trans_state right in
  match op with
  | Add -> Llvm.build_add lowered_left lowered_right "" trans_state.builder
  | Subtract -> Llvm.build_sub lowered_left lowered_right "" trans_state.builder
  | Multiply -> Llvm.build_mul lowered_left lowered_right "" trans_state.builder
  | Divide -> Llvm.build_sdiv lowered_left lowered_right "" trans_state.builder

and trans_lower_logical_op trans_state left op right =
  let lowered_left = trans_lower_expression trans_state left in
  let lowered_right = trans_lower_expression trans_state right in
  match op with
  | LogicalAnd -> Llvm.build_and lowered_left lowered_right "" trans_state.builder
  | LogicalOr -> Llvm.build_or lowered_left lowered_right "" trans_state.builder

and trans_lower_equality_op trans_state left op right =
  let lowered_left = trans_lower_expression trans_state left in
  let lowered_right = trans_lower_expression trans_state right in
  match op with
  | Equal -> Llvm.build_icmp Llvm.Icmp.Eq lowered_left lowered_right "" trans_state.builder
  | NotEqual -> Llvm.build_icmp Llvm.Icmp.Ne lowered_left lowered_right "" trans_state.builder

and trans_lower_comparison_op trans_state left op right =
  let lowered_left = trans_lower_expression trans_state left in
  let lowered_right = trans_lower_expression trans_state right in
  let icmp = match op with
    | GreaterThanEqual -> Llvm.Icmp.Sge
    | LessThanEqual -> Llvm.Icmp.Sle
    | GreaterThan -> Llvm.Icmp.Sgt
    | LessThan -> Llvm.Icmp.Slt in
  Llvm.build_icmp icmp lowered_left lowered_right "" trans_state.builder

and trans_lower_unop trans_state op target =
  let lowered_target = trans_lower_expression trans_state target in
  match op with
  | Not -> Llvm.build_not lowered_target "" trans_state.builder
  | UnaryMinus -> Llvm.build_neg lowered_target "" trans_state.builder

and trans_lower_new trans_state ty =
  let actual_ty = fst (Hashtbl.find trans_state.struct_field_table (ty_name_of ty)) in
  let size = Llvm.size_of actual_ty in
  let gc_malloc = unwrap (Llvm.lookup_function "shorty_gc_malloc" trans_state.llvm_mod) in
  let rtti = rtti_for trans_state ty in
  let ptr = Llvm.build_call gc_malloc [|size; rtti|] "" trans_state.builder in
  Llvm.build_bitcast ptr (llvm_type_of trans_state ty) "" trans_state.builder

let rec trans_lower_statement_list trans_state body =
  let terminated = ref false in
  let lower_statement = function
    | TypedReturn(expr) -> begin
        trans_lower_return trans_state expr;
        terminated := true;
      end
    | stmt -> trans_lower_statement trans_state stmt in
  List.iter lower_statement body;
  !terminated

and trans_lower_statement trans_state stmt = match stmt with
  | TypedDeclaration(name, binding) -> trans_lower_declaration trans_state name binding
  | TypedWhile(cond, body) -> trans_lower_while trans_state cond body
  | TypedIf(cond, true_br, false_br) -> trans_lower_if trans_state cond true_br false_br
  | TypedReturn(expr) -> trans_lower_return trans_state expr
  | TypedExpression(expr) -> trans_lower_expression_statement trans_state expr

and trans_lower_declaration trans_state name binding =
  let alloca = Llvm.build_alloca (llvm_type_of trans_state (type_of binding)) name trans_state.builder in
  let binding_val = trans_lower_expression trans_state binding in
  Hashtbl.add trans_state.identifier_map name alloca; 
  ignore (Llvm.build_store binding_val alloca trans_state.builder);

and trans_lower_while trans_state cond body =
  let body_block = Llvm.append_block trans_state.context "while_body" (trans_current_function trans_state) in
  let cond_block = Llvm.append_block trans_state.context "while_cond" (trans_current_function trans_state) in
  let exit_block = Llvm.append_block trans_state.context "while_exit" (trans_current_function trans_state) in
  (* to codegen a while loop, we emit something like this:
     ...
       br while_cond
     while_body:
       ...
     while_cond
       %cond = ...
       br %cond, while_body, while_exit
     while_exit:
       ...
  *)
  ignore (Llvm.build_br cond_block trans_state.builder);
  Llvm.position_at_end cond_block trans_state.builder;
  (* codegen the condition *)
  let cond_reg = trans_lower_expression trans_state cond in
  (* terminate this block with a conditional branch *)
  ignore (Llvm.build_cond_br cond_reg body_block exit_block trans_state.builder);
  Llvm.position_at_end body_block trans_state.builder;
  (* codegen the body *)
  let terminated = trans_lower_statement_list trans_state body in
  (* terminate this block by falling through to the cond, if we need to terminate *)
  if not terminated then begin
    ignore (Llvm.build_br cond_block trans_state.builder);
  end;
  (* move the builder to the exit block to prepare for the next statement *)
  Llvm.position_at_end exit_block trans_state.builder;


and trans_lower_if_then_else trans_state cond true_br false_br =
  let true_block = Llvm.append_block trans_state.context "true_branch" (trans_current_function trans_state) in
  let false_block = Llvm.append_block trans_state.context "false_branch" (trans_current_function trans_state) in
  let end_block = Llvm.append_block trans_state.context "exit_branch" (trans_current_function trans_state) in
  let cond_reg = trans_lower_expression trans_state cond in
  ignore (Llvm.build_cond_br cond_reg true_block false_block trans_state.builder);
  Llvm.position_at_end true_block trans_state.builder;
  let terminated = trans_lower_statement_list trans_state true_br in
  if not terminated then begin
    ignore (Llvm.build_br end_block trans_state.builder);
  end;
  Llvm.position_at_end false_block trans_state.builder;
  let terminated = trans_lower_statement_list trans_state false_br in
  if not terminated then begin
    ignore (Llvm.build_br end_block trans_state.builder);
  end;
  Llvm.position_at_end end_block trans_state.builder;

and trans_lower_if_then_only trans_state cond true_br =
  let true_block = Llvm.append_block trans_state.context "true_branch" (trans_current_function trans_state) in
  let end_block = Llvm.append_block trans_state.context "end_block" (trans_current_function trans_state) in
  let cond_reg = trans_lower_expression trans_state cond in
  ignore (Llvm.build_cond_br cond_reg true_block end_block trans_state.builder);
  Llvm.position_at_end true_block trans_state.builder;
  let terminated = trans_lower_statement_list trans_state true_br in
  if not terminated then begin
    ignore (Llvm.build_br end_block trans_state.builder);
  end;
  Llvm.position_at_end end_block trans_state.builder;

and trans_lower_if trans_state cond true_br false_br = match false_br with
  | Some br -> trans_lower_if_then_else trans_state cond true_br br
  | None -> trans_lower_if_then_only trans_state cond true_br

and trans_lower_return trans_state expr = match expr with
  (* shortyc does not do flow analysis, so unreachable code is possible.
     We just assume for now that it doesn't happen. This compiler isn't intended to
     be robust in the sad path. LLVM does super weird stuff when you build instructions
     past the block terminator, so I'm satisfied just calling it undefined behavior. *)
  | Some value -> begin
      let expr_reg = trans_lower_expression trans_state value in
      let actual_expr_reg = if ty_is_nil (type_of value) then begin
          Llvm.build_bitcast expr_reg
            (unwrap trans_state.current_fn_return_ty)
            ""
            trans_state.builder
        end else expr_reg in
      ignore (Llvm.build_store actual_expr_reg (unwrap trans_state.current_fn_return_value) trans_state.builder);
      ignore (Llvm.build_br (unwrap trans_state.current_fn_exit_block) trans_state.builder);
    end
  | None -> begin
      ignore (Llvm.build_br (unwrap trans_state.current_fn_exit_block) trans_state.builder);
    end;

and trans_lower_expression_statement trans_state expr = ignore (trans_lower_expression trans_state expr)

let trans_build_ret_val_slot context retty =
  if ty_is_void retty then None
  else Some(Llvm.build_alloca (llvm_type_of context retty) "retval" context.builder)

let trans_terminate_function context = match context.current_fn_return_value with
  | Some value -> begin
      let actual_ret = Llvm.build_load value "" context.builder in
      ignore (Llvm.build_ret actual_ret context.builder);
    end
  | None -> ignore (Llvm.build_ret_void context.builder)

let trans_lower_parameters trans_state func params =
  (* for every parameter, we introduce a new local in the
     entry block of the function and assign the parameter
     to this local. This greatly simplifies the code for
     reading from local variables by unifying the approach
     of local variables and parameters *)
  let lower_single_parameter trans_state llval param =
    let (name, llty) = param in
    let local = Llvm.build_alloca llty name trans_state.builder in
    Hashtbl.add trans_state.identifier_map name local;
    ignore (Llvm.build_store llval local trans_state.builder) in

  let param_llvals = Array.to_list (Llvm.params func) in
  let param_name_and_ty = List.map (fun (name, ty) -> (name, llvm_type_of trans_state ty)) params in
  List.iter2 (lower_single_parameter trans_state) param_llvals param_name_and_ty;;

let trans_lower_fn_decl trans_state name params retty body =
  let param_tys = List.map (fun (_, ty) -> ty) params in
  let fun_ty = llvm_type_of_function trans_state param_tys retty in
  let func = Llvm.define_function name fun_ty trans_state.llvm_mod in
  Hashtbl.add trans_state.identifier_map name func;
  (* cc 1 == fastcall *)
  Llvm.set_function_call_conv 1 func;
  Llvm.set_gc (Some("statepoint-example")) func;
  let entry_block = Llvm.entry_block func in
  let builder = Llvm.builder_at_end trans_state.context entry_block in
  trans_state.builder <- builder;
  Llvm.position_at_end entry_block trans_state.builder;
  if name = "main" then begin
    (* for the entry point, we need to emit a call to shorty_runtime_init *)
    let init = unwrap (Llvm.lookup_function "shorty_runtime_init" trans_state.llvm_mod) in
    ignore (Llvm.build_call init [||] "" trans_state.builder);
  end;
  trans_lower_parameters trans_state func params;
  let ret_value = trans_build_ret_val_slot trans_state retty in
  let first_block = Llvm.append_block trans_state.context "first" func in
  ignore (Llvm.build_br first_block trans_state.builder);
  Llvm.position_at_end first_block trans_state.builder;
  let exit_block = Llvm.append_block trans_state.context "exit" func in
  trans_state.current_fn_exit_block <- Some(exit_block);
  trans_state.current_fn_return_value <- ret_value;
  trans_state.current_fn_return_ty <- Some(llvm_type_of trans_state retty);
  let terminated = trans_lower_statement_list trans_state body in
  if not terminated then begin
    ignore (Llvm.build_br exit_block trans_state.builder);
  end;
  Llvm.position_at_end exit_block trans_state.builder;
  trans_terminate_function trans_state;;
  (*Llvm_analysis.view_function_cfg func;;*)


let trans_lower_extern_fn_decl trans_state name params retty =
  let param_tys = List.map snd params in
  let ty = llvm_type_of_function trans_state param_tys retty in
  let func = Llvm.declare_function name ty trans_state.llvm_mod in
  Hashtbl.add trans_state.identifier_map name func;
  (* cc 0 = cdecl, the assumed cconv of extern functions *)
  Llvm.set_function_call_conv 0 func;;

let trans_declare_rtti_struct trans_state =
  (*
     struct type_info {
        size_t num_pointers;
        size_t* pointers;
        const char* type_name;
     }
   *)
  let struct_ty = Llvm.named_struct_type trans_state.context "__type_info" in
  let num_pointers = Llvm.i64_type trans_state.context in
  let pointers = Llvm.pointer_type (Llvm.i64_type trans_state.context) in
  let name = Llvm.pointer_type (Llvm.i8_type trans_state.context) in
  Llvm.struct_set_body struct_ty [|num_pointers; pointers; name|] false;
  trans_state.rtti_struct <- Some(struct_ty);;

let trans_generate_type_info trans_state name fields =
  (* it is the compiler's responsibility to generate a
     data structure that will be used by the runtime to precisely
     identify the location of pointers in a type.

     The type info has this layout:
     struct type_info {
        size_t num_pointers;
        size_t* pointers;
        const char* type_name;
     }

     We have to generate three global values here: one, an array
     containing the offsets of every pointer in the type, the
     name of this type, and this struct itself. *)
  let rec find_pointers = function
    | [] -> []
    | (field_name, ty) :: tail -> if ty_is_struct ty
      then trans_state_get_struct_offset trans_state name field_name :: find_pointers tail
      else find_pointers tail in
  let actual_offset_of = function
    | 0 -> Llvm.const_int (Llvm.i64_type trans_state.context) 0
    | offset -> begin
    let struct_ty = llvm_type_of trans_state name in
    let null_ptr = Llvm.const_null struct_ty in
    let indices = [|Llvm.const_int (Llvm.i32_type trans_state.context) 0; Llvm.const_int (Llvm.i32_type trans_state.context) offset|] in
    let gep = Llvm.const_gep null_ptr indices in
    Llvm.const_ptrtoint gep (Llvm.i64_type trans_state.context)
  end in
  (*let actual_offset_of offset = offset in*)
  let pointers = find_pointers fields in
  let num_pointers = Llvm.const_int (Llvm.i64_type trans_state.context) (List.length pointers) in
  let pointer_offsets = List.map actual_offset_of pointers in
  let pointers = Llvm.const_array (Llvm.i64_type trans_state.context) (Array.of_list pointer_offsets) in
  let pointers_global = Llvm.define_global ("__" ^ (ty_name_of name) ^ "_pointer_table") pointers trans_state.llvm_mod in
  let indices = [|Llvm.const_int (Llvm.i64_type trans_state.context) 0; Llvm.const_int (Llvm.i64_type trans_state.context) 0|] in
  let pointers_gep = Llvm.const_gep pointers_global indices in
  let ty_name = Llvm.const_stringz trans_state.context (ty_name_of name) in
  let ty_name_global = Llvm.define_global ("__" ^ (ty_name_of name) ^ "_name") ty_name trans_state.llvm_mod in
  let ty_name_gep = Llvm.const_gep ty_name_global indices in
  let struct_fields = [|num_pointers; pointers_gep; ty_name_gep|] in
  let type_info_initializer = Llvm.const_named_struct (unwrap trans_state.rtti_struct) struct_fields in
  let type_info_name = "__" ^ (ty_name_of name) ^ "_type_info" in
  Llvm.define_global type_info_name type_info_initializer trans_state.llvm_mod


let trans_lower_struct_decl trans_state name fields =
  let struct_ty = Llvm.named_struct_type trans_state.context name in
  trans_state_add_struct trans_state struct_ty name fields;
  let field_types = List.map snd fields in
  let llvm_tys = List.map (llvm_type_of trans_state) field_types in
  let llvm_tys_array = Array.of_list llvm_tys in
  Llvm.struct_set_body struct_ty llvm_tys_array false;
  ignore (trans_generate_type_info trans_state (Named(name)) fields);;


let trans_lower_top_level_decl trans_state decl = match decl with
  | TypedFnDecl(name, params, retty, body) -> trans_lower_fn_decl trans_state name params retty body
  | TypedExternFnDecl(name, params, retty) -> trans_lower_extern_fn_decl trans_state name params retty
  | TypedStructDecl(name, fields) -> trans_lower_struct_decl trans_state name fields

let trans_declare_runtime_functions trans_state =
  (* there are a few functions that the compiler expects that the runtime will provide.
     Right now, there are only two:
       1) void shorty_runtime_init(void), called to initialize the runtime, and
       2) void *shorty_gc_malloc(size_t), called by new to allocate a new object. *)
  let runtime_init_ty = Llvm.function_type (Llvm.void_type trans_state.context) [||] in
  let gc_malloc_ty = Llvm.function_type (Llvm.qualified_pointer_type (Llvm.i8_type trans_state.context) 1) [|Llvm.i64_type trans_state.context; Llvm.pointer_type (Llvm.i8_type trans_state.context)|] in
  let runtime_init = Llvm.declare_function "shorty_runtime_init" runtime_init_ty trans_state.llvm_mod in
  let gc_malloc = Llvm.declare_function "shorty_gc_malloc" gc_malloc_ty trans_state.llvm_mod in
  (* all of our runtime functions use the C calling convention *)
  Llvm.set_function_call_conv 0 runtime_init;
  Llvm.set_function_call_conv 0 gc_malloc;;

let trans_lower_compilation_unit name unit : Llvm.llmodule =
  let context = Llvm.global_context () in
  let llvm_mod = Llvm.create_module context name in
  let builder = Llvm.builder context in
  let trans_state = trans_state_create context llvm_mod builder in
  trans_declare_runtime_functions trans_state;
  trans_declare_rtti_struct trans_state;
  List.iter (trans_lower_top_level_decl trans_state) unit;
  trans_state.llvm_mod



