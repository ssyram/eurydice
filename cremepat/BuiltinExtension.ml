(* =================================================================================================
   CREMEPAT BUILTIN EXTENSION: PPX Integration for Builtin Function Calls
   =================================================================================================
   
   This module provides the PPX extension point for cremepat builtin function calls.
   It integrates parsing, validation, and direct compilation to Krml expressions,
   providing the [%cremebuiltin {| ... |}] syntax.
   
   ARCHITECTURE: Parse → Validate → Compile → Generate OCaml → Integrate
   
   The design is intentionally simple, focusing on direct transformation from
   builtin calls to Krml AST construction code.
 *)

open Ppxlib
open BuiltinExpr

(* =================================================================================================
   ERROR HANDLING WITH LOCATION INFORMATION
   ================================================================================================= *)

(* Create located error for PPX *)
let located_error ~loc message =
  Location.error_extensionf ~loc "cremebuiltin error: %s" message

(* Create located warning *)
let located_warning ~loc message =
  Location.error_extensionf ~loc "cremebuiltin warning: %s" message

(* Format validation error with helpful context *)
let format_validation_error call error_msg =
  Printf.sprintf "%s\nCall: %s\nAvailable builtins: %s"
    error_msg
    (string_of_builtin_call call)
    (String.concat ", " (available_builtins ()))

(* =================================================================================================
   KRML AST GENERATION
   ================================================================================================= *)

(* Generate OCaml expression that constructs Krml type *)
let generate_krml_type ~loc type_name =
  let open Ast_builder.Default in
  match type_name with
  | "i32" -> [%expr Krml.Ast.TInt Krml.Ast.I32]
  | "u32" -> [%expr Krml.Ast.TInt Krml.Ast.U32] 
  | "i64" -> [%expr Krml.Ast.TInt Krml.Ast.I64]
  | "u64" -> [%expr Krml.Ast.TInt Krml.Ast.U64]
  | "usize" -> [%expr Krml.Ast.TInt Krml.Ast.SizeT]
  | "bool" -> [%expr Krml.Ast.TBool]
  | "unit" -> [%expr Krml.Ast.TUnit]
  | type_var -> 
      (* For generic types, generate TBound reference *)
      [%expr Krml.Ast.TBound 0] (* Simplified - real implementation would track type variable indices *)

(* Generate OCaml expression that constructs Krml qualified name *)
let generate_krml_qualified_name ~loc (module_path, name) =
  let open Ast_builder.Default in
  let module_list = List.fold_right (fun m acc ->
    [%expr [%e estring ~loc m] :: [%e acc]]
  ) module_path [%expr []] in
  [%expr ([%e module_list], [%e estring ~loc name])]

(* Generate OCaml expression for builtin argument *)
let generate_krml_argument ~loc = function
  | PatternVar var_name ->
      (* Pattern variable becomes OCaml variable reference *)
      let open Ast_builder.Default in
      pexp_ident ~loc { txt = Lident var_name; loc }
  
  | Literal (IntLit n) ->
      (* Integer literal becomes Krml constant *)
      let open Ast_builder.Default in
      [%expr Krml.Ast.with_type (Krml.Ast.TInt Krml.Ast.I32) 
               (Krml.Ast.EConstant (Krml.Ast.Int (Krml.Ast.I32, [%e estring ~loc (string_of_int n)])))]
  
  | Literal (BoolLit b) ->
      (* Boolean literal becomes Krml boolean *)
      let open Ast_builder.Default in
      [%expr Krml.Ast.with_type Krml.Ast.TBool (Krml.Ast.EBool [%e ebool ~loc b])]
  
  | Literal (StringLit s) ->
      (* String literal becomes Krml string *)
      let open Ast_builder.Default in
      [%expr Krml.Ast.with_type Krml.Ast.TStr (Krml.Ast.EString [%e estring ~loc s])]

(* =================================================================================================
   MAIN COMPILATION FUNCTIONS
   ================================================================================================= *)

(* Generate Krml expression for builtin function call *)
let compile_builtin_call ~loc call builtin_info =
  let open Ast_builder.Default in
  
  (* Generate qualified function name *)
  let func_name_expr = generate_krml_qualified_name ~loc builtin_info.krml_name in
  
  (* Generate argument expressions *)
  let arg_exprs = List.map (generate_krml_argument ~loc) call.args in
  let args_list = List.fold_right (fun arg acc ->
    [%expr [%e arg] :: [%e acc]]
  ) arg_exprs [%expr []] in
  
  (* Generate type arguments if present *)
  let type_args_list = 
    if call.type_args = [] then [%expr []]
    else
      let type_exprs = List.map (generate_krml_type ~loc) call.type_args in
      List.fold_right (fun typ acc ->
        [%expr [%e typ] :: [%e acc]]
      ) type_exprs [%expr []]
  in
  
  (* Generate the complete Krml function call *)
  if call.type_args = [] then
    (* Simple function call without type arguments *)
    [%expr 
      let func_expr = Krml.Ast.with_type Krml.Ast.TAny (Krml.Ast.EQualified [%e func_name_expr]) in
      Krml.Ast.with_type Krml.Ast.TAny (Krml.Ast.EApp (func_expr, [%e args_list]))
    ]
  else
    (* Function call with type arguments (using ETApp) *)
    [%expr 
      let base_func = Krml.Ast.with_type Krml.Ast.TAny (Krml.Ast.EQualified [%e func_name_expr]) in
      let typed_func = Krml.Ast.with_type Krml.Ast.TAny 
        (Krml.Ast.ETApp (base_func, [], [], [%e type_args_list])) in
      Krml.Ast.with_type Krml.Ast.TAny (Krml.Ast.EApp (typed_func, [%e args_list]))
    ]

(* =================================================================================================
   PPX EXPANSION FUNCTION
   ================================================================================================= *)

(* Main expansion function for [%cremebuiltin {| ... |}] *)
let expand_builtin ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  
  match payload with
  | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (input, _, _)); _ }, _); _ }] ->
      
      (* Parse the input string *)
      (match BuiltinParser.parse_and_validate input with
      | Error parse_error ->
          located_error ~loc parse_error
      
      | Ok (call, builtin_info) ->
          try
            (* Generate Krml expression construction code *)
            compile_builtin_call ~loc call builtin_info
          with
          | exn ->
              located_error ~loc 
                (Printf.sprintf "Code generation failed: %s" (Printexc.to_string exn)))
  
  | _ ->
      located_error ~loc 
        "cremebuiltin extension expects a string literal: [%cremebuiltin {| function_call |}]"

(* =================================================================================================
   PPX EXTENSION REGISTRATION
   ================================================================================================= *)

(* PPX extension declaration *)
let builtin_extension =
  Extension.V3.declare "cremebuiltin" Expression
    Ast_pattern.(single_expr_payload (estring __))
    expand_builtin

(* Create rule and register with driver *)
let builtin_rule = Context_free.Rule.extension builtin_extension

(* Register the extension *)
let () = 
  Driver.register_transformation 
    ~rules:[builtin_rule] 
    "cremepat_builtins"

(* =================================================================================================
   TESTING AND DEBUGGING UTILITIES
   ================================================================================================= *)

(* Test compilation of builtin call *)
let test_compile_builtin input =
  let loc = Location.none in
  try
    match BuiltinParser.parse_and_validate input with
    | Error msg ->
        Printf.printf "Parse/validation failed: %s\n" msg
    | Ok (call, builtin_info) ->
        Printf.printf "Compiling: %s\n" (string_of_builtin_call call);
        let _result = compile_builtin_call ~loc call builtin_info in
        Printf.printf "Compilation successful\n"
  with
  | exn ->
      Printf.printf "Compilation failed: %s\n" (Printexc.to_string exn)

(* Debug function showing complete pipeline *)
let debug_builtin_compilation input =
  Printf.printf "=== Debugging builtin compilation ===\n";
  Printf.printf "Input: %s\n" input;
  
  (* Stage 1: Parsing *)
  (match BuiltinParser.parse_string input with
  | Error parse_error ->
      Printf.printf "Parse error: %s\n" parse_error
  | Ok call ->
      Printf.printf "Parse successful: %s\n" (string_of_builtin_call call);
      
      (* Stage 2: Validation *)
      (match validate_builtin_call call with
      | Error validation_error ->
          Printf.printf "Validation error: %s\n" validation_error
      | Ok builtin_info ->
          Printf.printf "Validation successful: %s\n" builtin_info.description;
          
          (* Stage 3: Compilation *)
          Printf.printf "Would generate Krml AST construction code\n"));
  
  Printf.printf "=== End debug ===\n"

(* =================================================================================================
   USAGE EXAMPLES AND DOCUMENTATION
   ================================================================================================= *)

(*
   USAGE EXAMPLES:
   
   Basic builtin calls:
   ```ocaml
   let slice_access = [%cremebuiltin {| slice_index(?slice, ?index) |}]
   let new_vec = [%cremebuiltin {| vec_new::<i32>() |}]
   let discriminant_value = [%cremebuiltin {| discriminant::<MyEnum, u32>(?enum_val) |}]
   ```
   
   Generated code (conceptual):
   ```ocaml
   let slice_access = 
     let func_expr = Krml.Ast.with_type Krml.Ast.TAny 
       (Krml.Ast.EQualified (["Eurydice"], "slice_index")) in
     Krml.Ast.with_type Krml.Ast.TAny (Krml.Ast.EApp (func_expr, [slice; index]))
   ```
   
   INTEGRATION WITH EXISTING CREMEPAT:
   
   Pattern matching can capture values that are then used in builtin calls:
   ```ocaml
   match expr with
   | [%cremepat {| slice_index(?s, ?i) |}] ->
       (* Use captured s and i in new builtin call *)
       [%cremebuiltin {| array_to_slice(?s) |}]
   ```
   
   CURRENT LIMITATIONS:
   - Only builtin functions from Builtin.ml
   - No complex expressions or composition
   - Simple type argument handling
   - Pattern variables only, no complex variable references
   
   EXTENSION PATH:
   - Add more builtin functions
   - Support variable references
   - Enable composition: builtin1(builtin2(?x))
   - Integration with let bindings
*)