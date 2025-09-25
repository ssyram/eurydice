(* =================================================================================================
   CREMEPAT EXPRESSION COMPILER: Generate Krml AST from Cremepat Expressions
   =================================================================================================
   
   This module compiles type-checked cremepat expressions into Krml.Ast expressions.
   The key challenge is bridging between cremepat's simplified type system and
   Krml's full OCaml-style typed AST.
   
   COMPILATION STRATEGY:
   - Use type information to generate well-typed Krml expressions
   - Handle pattern variables by generating OCaml expressions that construct AST
   - Support both concrete expressions and mixed concrete/pattern expressions
   - Generate code that integrates seamlessly with existing Krml usage
 *)

open ExprParseTree
module K = Krml.Ast

(* =================================================================================================
   COMPILATION CONTEXT AND UTILITIES
   ================================================================================================= *)

(* Compilation context *)
type compile_ctx = {
  (* Current module path for name resolution *)
  module_path : string list;
  
  (* Variable environment mapping names to De Bruijn indices *)
  var_env : (string * int) list;
  
  (* Type environment for generating type annotations *)
  type_env : type_env;
  
  (* Location for error reporting *)
  location : Ppxlib.Location.t;
  
  (* Counter for generating unique identifiers *)
  unique_counter : int ref;
}

(* Generate unique identifier *)
let fresh_name ctx prefix =
  let n = !(ctx.unique_counter) in
  ctx.unique_counter := n + 1;
  prefix ^ "_" ^ string_of_int n

(* =================================================================================================
   TYPE COMPILATION: ExprParseTree types → Krml types
   ================================================================================================= *)

(* Convert cremepat type to Krml type *)
let rec compile_type ctx typ_info =
  match typ_info.typ with
  | TInt K.I32 -> K.TInt K.I32
  | TInt width -> K.TInt width
  | TBool -> K.TBool
  | TStr -> K.TStr
  | TUnit -> K.TUnit
  | TVar name -> 
      (* TODO: Proper type variable handling *)
      K.TAny  (* Placeholder *)
  | TQualified (path, name) ->
      K.TQualified (path, name)
  | TApp (base_typ, arg_typs) ->
      let base = compile_type ctx { typ_info with typ = base_typ } in
      let args = List.map (fun t -> compile_type ctx { typ_info with typ = t }) arg_typs in
      K.TApp (base, args)

(* =================================================================================================
   LITERAL COMPILATION
   ================================================================================================= *)

(* Compile literals to Krml constants *)
let compile_literal ctx literal =
  match literal with
  | LInt (n, width_opt) ->
      let width = match width_opt with Some w -> w | None -> K.I32 in
      let typ = K.TInt width in
      K.with_type typ (K.EConstant (K.Int (width, string_of_int n)))
  
  | LBool b ->
      K.with_type K.TBool (K.EBool b)
  
  | LStr s ->
      K.with_type K.TStr (K.EString s)
  
  | LUnit ->
      K.with_type K.TUnit (K.EUnit)

(* =================================================================================================
   VARIABLE AND NAME RESOLUTION
   ================================================================================================= *)

(* Compile variable reference *)
let compile_variable ctx name typ_info =
  (* Look up De Bruijn index *)
  match List.assoc_opt name ctx.var_env with
  | Some index ->
      let krml_typ = compile_type ctx typ_info in
      K.with_type krml_typ (K.EBound index)
  | None ->
      (* This should have been caught in type inference *)
      failwith ("Unbound variable in compilation: " ^ name)

(* Compile qualified name *)
let compile_qualified_name ctx path typ_info =
  let krml_typ = compile_type ctx typ_info in
  K.with_type krml_typ (K.EQualified path)

(* =================================================================================================
   FUNCTION APPLICATION COMPILATION
   ================================================================================================= *)

(* Compile function application with full generic support *)
let compile_function_app ctx func type_args const_args args return_type =
  (* Compile function expression *)
  let func_expr = compile_expr ctx func in
  
  (* Compile type arguments *)
  let krml_type_args = List.map (compile_type ctx) type_args in
  
  (* Compile const generic arguments *)
  let krml_const_args = List.map (compile_expr ctx) const_args in
  
  (* Compile regular arguments *)
  let krml_args = List.map (compile_expr ctx) args in
  
  (* Build the application *)
  let krml_return_type = compile_type ctx return_type in
  let base_app = 
    if krml_type_args = [] && krml_const_args = [] then
      func_expr
    else
      K.with_type (K.TArrow (List.map (fun e -> e.K.typ) krml_args, krml_return_type))
        (K.ETApp (func_expr, krml_const_args, [], krml_type_args))  (* Adjusted for Krml's ETApp *)
  in
  
  K.with_type krml_return_type (K.EApp (base_app, krml_args))

(* =================================================================================================
   LET BINDING COMPILATION
   ================================================================================================= *)

(* Compile let expression *)
and compile_let_binding ctx var var_type init body =
  (* Compile initializer *)
  let init_expr = compile_expr ctx init in
  
  (* Extend environment with new binding *)
  let new_index = List.length ctx.var_env in
  let ctx' = { ctx with var_env = (var, new_index) :: ctx.var_env } in
  
  (* Compile body in extended environment *)
  let body_expr = compile_expr ctx' body in
  
  (* Create let expression *)
  K.with_type body_expr.K.typ (K.ELet (
    K.{ name = var; typ = init_expr.K.typ; mut = false },
    init_expr,
    body_expr
  ))

(* =================================================================================================
   MAIN EXPRESSION COMPILATION
   ================================================================================================= *)

(* Compile expression node to Krml expression *)
and compile_expr ctx expr =
  match expr.node with
  
  (* Literals *)
  | ELit literal ->
      compile_literal ctx literal
  
  (* Variables *)
  | EVar name ->
      (match expr.typ with
      | Some typ -> compile_variable ctx name typ
      | None -> failwith "Untyped variable in compilation")
  
  (* Qualified names *)
  | EQualified path ->
      (match expr.typ with
      | Some typ -> compile_qualified_name ctx path typ
      | None -> failwith "Untyped qualified name in compilation")
  
  (* Function application *)
  | EApp { func; type_args; const_args; args } ->
      (match expr.typ with
      | Some return_type -> 
          compile_function_app ctx func type_args const_args args return_type
      | None -> failwith "Untyped function application in compilation")
  
  (* Let bindings *)
  | ELet { var; var_type; init; body } ->
      compile_let_binding ctx var var_type init body
  
  (* Type ascription - just compile the inner expression *)
  | EAscribe (inner_expr, _) ->
      compile_expr ctx inner_expr
  
  (* Sequence expressions *)
  | ESequence exprs ->
      (match exprs with
      | [] -> K.with_type K.TUnit K.EUnit
      | [single] -> compile_expr ctx single
      | first :: rest ->
          let first_expr = compile_expr ctx first in
          let rest_expr = compile_expr ctx { expr with node = ESequence rest } in
          K.with_type rest_expr.K.typ (K.ESequence [first_expr; rest_expr]))
  
  (* Placeholders for complex expressions *)
  | EIf (cond, then_expr, else_expr) ->
      (* TODO: Implement if expressions *)
      failwith "If expressions not yet implemented"
  
  | EMatch (expr, branches) ->
      (* TODO: Implement match expressions *)
      failwith "Match expressions not yet implemented"
  
  | EWhile (cond, body) ->
      let cond_expr = compile_expr ctx cond in
      let body_expr = compile_expr ctx body in
      K.with_type K.TUnit (K.EWhile (cond_expr, body_expr))
  
  | ERecord fields ->
      (* TODO: Implement record expressions *)
      failwith "Record expressions not yet implemented"
  
  | ETuple exprs ->
      let compiled_exprs = List.map (compile_expr ctx) exprs in
      let tuple_type = K.TTuple (List.map (fun e -> e.K.typ) compiled_exprs) in
      K.with_type tuple_type (K.ETuple compiled_exprs)
  
  | EArray exprs ->
      (* TODO: Implement array expressions *)
      failwith "Array expressions not yet implemented"
  
  | EAddrOf expr ->
      let inner = compile_expr ctx expr in
      K.with_type (K.TBuf (inner.K.typ, false)) (K.EAddrOf inner)
  
  | EDeref expr ->
      (* TODO: Implement dereference *)
      failwith "Dereference expressions not yet implemented"
  
  | EIndex (array, index) ->
      let array_expr = compile_expr ctx array in
      let index_expr = compile_expr ctx index in
      (* TODO: Extract element type from array type *)
      let element_type = K.TAny in  (* Placeholder *)
      K.with_type element_type (K.EBufRead (array_expr, index_expr))
  
  | EField (obj, field) ->
      (* TODO: Implement field access *)
      failwith "Field access not yet implemented"
  
  | EMethodCall { receiver; method_name; type_args; args } ->
      (* TODO: Implement method calls *)
      failwith "Method calls not yet implemented"

(* =================================================================================================
   PATTERN VARIABLE COMPILATION: Mixed Concrete/Pattern Expressions
   ================================================================================================= *)

(* Compile expression with pattern variables to OCaml expression that builds Krml AST *)
let compile_expr_with_patterns ctx expr_with_vars =
  let open Ppxlib.Ast_builder.Default in
  let loc = ctx.location in
  
  match expr_with_vars with
  | Fixed expr_node ->
      (* Concrete expression - compile normally and quote the result *)
      let expr = { 
        node = expr_node; 
        typ = None;  (* Will be inferred *)
        meta = { location = None; annotations = [] } 
      } in
      let krml_expr = compile_expr ctx expr in
      (* Generate OCaml code that constructs this Krml expression *)
      [%expr K.with_type [%e compile_type_to_ocaml ctx krml_expr.K.typ] 
                        [%e compile_expr_node_to_ocaml ctx krml_expr.K.node]]
  
  | PatternVar (name, _typ_opt) ->
      (* Pattern variable - generate OCaml variable reference *)
      pexp_ident ~loc { txt = Lident name; loc }
  
  | ListPatternVar (name, _typ_opt) ->
      (* List pattern variable - needs special handling in context *)
      failwith "List pattern variables in expressions not yet implemented"

(* Helper: Compile Krml type to OCaml expression that constructs it *)
and compile_type_to_ocaml ctx krml_type =
  let open Ppxlib.Ast_builder.Default in
  let loc = ctx.location in
  
  match krml_type with
  | K.TInt K.I32 -> [%expr K.TInt K.I32]
  | K.TBool -> [%expr K.TBool]
  | K.TUnit -> [%expr K.TUnit]
  (* TODO: Complete type compilation *)
  | _ -> [%expr K.TAny]  (* Placeholder *)

(* Helper: Compile Krml expression node to OCaml expression that constructs it *)
and compile_expr_node_to_ocaml ctx node =
  let open Ppxlib.Ast_builder.Default in
  let loc = ctx.location in
  
  match node with
  | K.EConstant (K.Int (width, s)) -> 
      [%expr K.EConstant (K.Int ([%e [%expr K.I32]], [%e estring ~loc s]))]
  | K.EBool b -> 
      [%expr K.EBool [%e ebool ~loc b]]
  | K.EBound i -> 
      [%expr K.EBound [%e eint ~loc i]]
  (* TODO: Complete expression compilation *)
  | _ -> [%expr K.EUnit]  (* Placeholder *)

(* =================================================================================================
   PUBLIC API
   ================================================================================================= *)

(* Create compilation context *)
let create_compile_ctx module_path type_env location = {
  module_path;
  var_env = [];
  type_env;
  location;
  unique_counter = ref 0;
}

(* Compile typed expression to Krml AST *)
let compile_typed_expression module_path type_env location expr =
  let ctx = create_compile_ctx module_path type_env location in
  try
    Ok (compile_expr ctx expr)
  with
  | Failure msg -> Error ("Compilation error: " ^ msg)
  | exn -> Error ("Unexpected error: " ^ Printexc.to_string exn)

(* Compile expression with pattern variables to OCaml code *)
let compile_mixed_expression module_path type_env location expr_with_vars =
  let ctx = create_compile_ctx module_path type_env location in
  try
    Ok (compile_expr_with_patterns ctx expr_with_vars)
  with
  | Failure msg -> Error ("Pattern compilation error: " ^ msg)
  | exn -> Error ("Unexpected error: " ^ Printexc.to_string exn)

(* =================================================================================================
   TESTING AND DEBUGGING UTILITIES
   ================================================================================================= *)

(* Pretty print compiled expression for debugging *)
let debug_compile_expression module_path type_env location expr =
  match compile_typed_expression module_path type_env location expr with
  | Ok krml_expr -> 
      Printf.printf "Compiled successfully\n";
      (* TODO: Add pretty printer for Krml expressions *)
      krml_expr
  | Error msg ->
      Printf.printf "Compilation failed: %s\n" msg;
      failwith msg

(* =================================================================================================
   IMPLEMENTATION ROADMAP
   ================================================================================================= *)

(*
   CURRENT STATUS:
   ✓ Basic framework and types
   ✓ Literal compilation
   ✓ Variable and qualified name compilation  
   ✓ Simple function application
   ✓ Let bindings
   ✓ Basic pattern variable support
   
   TODO - PHASE 1 (Basic expressions):
   - Complete type compilation (all Krml types)
   - Improve error handling and location tracking
   - Add comprehensive testing
   - Integrate with PPX extension
   
   TODO - PHASE 2 (Control flow):
   - If expressions
   - Match expressions with pattern integration
   - While loops with proper typing
   - Sequence expressions
   
   TODO - PHASE 3 (Data structures):
   - Record expressions and field access
   - Tuple expressions  
   - Array expressions and indexing
   - Address-of and dereference operators
   
   TODO - PHASE 4 (Advanced features):
   - Method call resolution
   - Generic instantiation
   - Const generic evaluation
   - Integration with existing cremepat patterns
   
   TESTING STRATEGY:
   - Unit tests for each expression type
   - Integration tests with type inference
   - Round-trip tests (parse → compile → execute)
   - Error handling tests
   - Performance benchmarks vs manual construction
*)