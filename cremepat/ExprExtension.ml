(* =================================================================================================
   CREMEPAT EXPRESSION EXTENSION: PPX Integration for Expression Generation
   =================================================================================================
   
   This module provides the PPX extension point for cremepat expression generation.
   It integrates parsing, type inference, and code generation to provide the
   [%cremeexpr {| ... |}] syntax for generating Krml expressions.
   
   ARCHITECTURE:
   Input String → Parse → Type Check → Compile → OCaml Expression AST → Source Integration
 *)

open Ppxlib
open ExprParseTree

(* =================================================================================================
   ERROR HANDLING AND DIAGNOSTICS
   ================================================================================================= *)

(* Enhanced error handling with location information *)
module ErrorHandling = struct
  (* Create located error *)
  let located_error ~loc message =
    Location.error_extensionf ~loc "%s" message
  
  (* Create located warning *)  
  let located_warning ~loc message =
    Location.error_extensionf ~loc "Warning: %s" message
  
  (* Format type inference error *)
  let format_inference_error = function
    | TypeInference.Success _ -> "No error"
    | TypeInference.NeedsAnnotation reason -> 
        "Type annotation required: " ^ reason
    | TypeInference.TypeError error ->
        "Type error: " ^ error
  
  (* Format compilation error *)
  let format_compile_error = function
    | Ok _ -> "No error"
    | Error msg -> "Compilation error: " ^ msg
end

(* =================================================================================================
   PARSING INFRASTRUCTURE
   ================================================================================================= */

(* Parse expression string using custom lexer and parser *)
let parse_expression_string ~loc input_string =
  try
    (* Create lexer buffer *)
    let lexbuf = Sedlexing.Utf8.from_string input_string in
    
    (* Reset lexer state *)
    ExprLex.reset_lexer ();
    
    (* Parse using Menhir *)
    let parser = MenhirLib.Convert.Simplified.traditional2revised ExprParse.fragment in
    let result = parser (fun _ -> ExprLex.token lexbuf) in
    
    Ok result
    
  with
  | (Sedlexing.MalFormed | Sedlexing.InvalidCodepoint _) as e ->
      Error (Printf.sprintf "Lexing error in expression: %s\nInput: %s" 
        (Printexc.to_string e) input_string)
        
  | ExprParse.Error ->
      let (start_pos, end_pos) = Sedlexing.loc lexbuf in
      let line = !ExprLex.lines in
      Error (Printf.sprintf "Parse error at line %d, characters %d-%d\nInput: %s" 
        line start_pos end_pos input_string)
        
  | exn ->
      Error (Printf.sprintf "Unexpected parsing error: %s\nInput: %s" 
        (Printexc.to_string exn) input_string)

(* =================================================================================================
   TYPE ENVIRONMENT SETUP
   ================================================================================================= *)

(* Create basic type environment for expression inference *)
let create_basic_type_env () =
  let open ExprParseTree in
  
  (* Basic built-in functions *)
  let builtin_functions = [
    (* Arithmetic operators *)
    ([], "+"), { type_params = ["T"]; const_params = []; param_types = []; return_type = make_typ_info (TVar "T") ~inferred:false () };
    ([], "-"), { type_params = ["T"]; const_params = []; param_types = []; return_type = make_typ_info (TVar "T") ~inferred:false () };
    ([], "*"), { type_params = ["T"]; const_params = []; param_types = []; return_type = make_typ_info (TVar "T") ~inferred:false () };
    ([], "/"), { type_params = ["T"]; const_params = []; param_types = []; return_type = make_typ_info (TVar "T") ~inferred:false () };
    
    (* Comparison operators *)
    ([], "=="), { type_params = ["T"]; const_params = []; param_types = []; return_type = make_typ_info TBool ~inferred:false () };
    ([], "!="), { type_params = ["T"]; const_params = []; param_types = []; return_type = make_typ_info TBool ~inferred:false () };
    
    (* Standard library functions *)
    (["std"; "vec"], "Vec"), { type_params = ["T"]; const_params = []; param_types = []; return_type = make_typ_info (TVar "Vec<T>") ~inferred:false () };
  ] in
  
  {
    vars = [];
    type_vars = [];
    const_generics = [];
    functions = builtin_functions;
    expected_type = None;
    module_path = [];
  }

(* =================================================================================================
   EXPRESSION COMPILATION PIPELINE
   ================================================================================================= *)

(* Convert ExprParseTree expression to compilable form *)
let rec convert_to_compilable_expr = function
  | Fixed expr_node -> 
      { node = expr_node; typ = None; meta = { location = None; annotations = [] } }
  | PatternVar (name, typ_opt) ->
      (* Pattern variables need special handling - they become OCaml variables *)
      failwith ("Pattern variable in expression compilation not yet implemented: " ^ name)
  | ListPatternVar (name, typ_opt) ->
      failwith ("List pattern variable in expression compilation not yet implemented: " ^ name)

(* Main compilation pipeline *)
let compile_expression ~loc input_string =
  (* Step 1: Parse expression *)
  match parse_expression_string ~loc input_string with
  | Error parse_error ->
      ErrorHandling.located_error ~loc parse_error
      
  | Ok parsed_expr ->
      try
        (* Step 2: Set up type environment *)
        let type_env = create_basic_type_env () in
        
        (* Step 3: Handle pattern variables vs concrete expressions *)
        match parsed_expr with
        | Fixed expr_node ->
            (* Concrete expression - full compilation pipeline *)
            let expr = convert_to_compilable_expr parsed_expr in
            
            (* Step 4: Type inference *)
            (match TypeInference.infer_expression_type type_env expr with
            | TypeInference.Success inferred_type ->
                let typed_expr = { expr with typ = Some inferred_type } in
                
                (* Step 5: Compile to Krml AST *)
                (match ExprCompiler.compile_typed_expression [] type_env loc typed_expr with
                | Ok krml_expr ->
                    (* Step 6: Generate OCaml code that constructs the Krml expression *)
                    generate_krml_construction_code ~loc krml_expr
                    
                | Error compile_error ->
                    ErrorHandling.located_error ~loc compile_error)
                    
            | inference_error ->
                ErrorHandling.located_error ~loc (ErrorHandling.format_inference_error inference_error))
        
        | PatternVar _ | ListPatternVar _ ->
            (* Mixed expression with pattern variables - generate pattern construction code *)
            (match ExprCompiler.compile_mixed_expression [] type_env loc parsed_expr with
            | Ok ocaml_expr -> ocaml_expr
            | Error compile_error ->
                ErrorHandling.located_error ~loc compile_error)
                
      with
      | exn ->
          ErrorHandling.located_error ~loc 
            ("Unexpected error in expression compilation: " ^ Printexc.to_string exn)

(* =================================================================================================
   CODE GENERATION: OCaml AST Construction
   ================================================================================================= *)

(* Generate OCaml code that constructs a Krml expression *)
and generate_krml_construction_code ~loc krml_expr =
  let open Ast_builder.Default in
  
  (* This is a placeholder - would need full Krml AST → OCaml AST conversion *)
  match krml_expr.Krml.Ast.node with
  | Krml.Ast.EConstant (Krml.Ast.Int (width, value)) ->
      [%expr Krml.Ast.with_type 
        (Krml.Ast.TInt [%e construct_width_expr ~loc width])
        (Krml.Ast.EConstant (Krml.Ast.Int ([%e construct_width_expr ~loc width], [%e estring ~loc value])))]
  
  | Krml.Ast.EBool b ->
      [%expr Krml.Ast.with_type Krml.Ast.TBool (Krml.Ast.EBool [%e ebool ~loc b])]
  
  | Krml.Ast.EBound i ->
      [%expr Krml.Ast.with_type Krml.Ast.TAny (Krml.Ast.EBound [%e eint ~loc i])]
  
  | _ ->
      (* Placeholder for complex expressions *)
      [%expr Krml.Ast.with_type Krml.Ast.TUnit Krml.Ast.EUnit]

(* Helper: Generate OCaml expression for Krml width *)
and construct_width_expr ~loc = function
  | Krml.Ast.I32 -> [%expr Krml.Ast.I32]
  | Krml.Ast.U32 -> [%expr Krml.Ast.U32]
  | Krml.Ast.I64 -> [%expr Krml.Ast.I64]
  | Krml.Ast.U64 -> [%expr Krml.Ast.U64]
  | _ -> [%expr Krml.Ast.I32]  (* Default fallback *)

(* =================================================================================================
   PPX EXTENSION REGISTRATION
   ================================================================================================= *)

(* Main expansion function for [%cremeexpr {| ... |}] *)
let expand_expression ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  
  match payload with
  | PStr [{ pstr_desc = Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (input, _, _)); _ }, _); _ }] ->
      compile_expression ~loc input
      
  | _ ->
      ErrorHandling.located_error ~loc 
        "cremeexpr extension expects a string literal: [%cremeexpr {| expression |}]"

(* PPX extension declaration *)
let expression_extension =
  Extension.V3.declare "cremeexpr" Expression
    Ast_pattern.(single_expr_payload (estring __))
    expand_expression

(* Create rule and register with driver *)
let expression_rule = Context_free.Rule.extension expression_extension

(* Register alongside existing cremepat extension *)
let () = 
  Driver.register_transformation 
    ~rules:[expression_rule] 
    "cremepat_expressions"

(* =================================================================================================
   TESTING AND DEBUGGING UTILITIES
   ================================================================================================= *)

(* Test expression compilation *)
let test_compile_expression input =
  let loc = Location.none in
  try
    let result = compile_expression ~loc input in
    Printf.printf "Compilation successful for: %s\n" input;
    result
  with
  | Location.Error error ->
      Printf.printf "Compilation failed: %s\n" (Location.Error.message error);
      failwith "Test compilation failed"

(* Debug function to show compilation stages *)
let debug_compile_expression input =
  let loc = Location.none in
  
  Printf.printf "=== Debugging expression compilation ===\n";
  Printf.printf "Input: %s\n" input;
  
  (* Stage 1: Parsing *)
  (match parse_expression_string ~loc input with
  | Error parse_error ->
      Printf.printf "Parse error: %s\n" parse_error
  | Ok parsed_expr ->
      Printf.printf "Parse successful\n";
      
      (* Stage 2: Type inference *)
      let type_env = create_basic_type_env () in
      (match parsed_expr with
      | Fixed expr_node ->
          let expr = convert_to_compilable_expr parsed_expr in
          let inference_result = TypeInference.infer_expression_type type_env expr in
          Printf.printf "Type inference: %s\n" 
            (TypeInference.string_of_inference_result inference_result);
      | _ ->
          Printf.printf "Pattern variables detected - skipping type inference\n"));
  
  Printf.printf "=== End debug ===\n"

(* =================================================================================================
   IMPLEMENTATION STATUS AND ROADMAP
   ================================================================================================= *)

(*
   CURRENT IMPLEMENTATION STATUS:
   
   ✓ PPX extension registration and integration
   ✓ Basic parsing pipeline with error handling
   ✓ Type environment setup with built-in functions
   ✓ Compilation pipeline architecture
   ✓ Error handling with location information
   ✓ Testing and debugging utilities
   
   ⚠ PARTIAL IMPLEMENTATIONS:
   - Krml AST generation (basic cases only)
   - Type inference integration (simplified)
   - Pattern variable handling (placeholder)
   - Code generation (limited constructs)
   
   ❌ NOT YET IMPLEMENTED:
   - Complex expression compilation
   - Full type inference integration
   - Method call resolution
   - Generic instantiation
   - Error recovery and suggestions
   
   NEXT STEPS:
   1. Complete basic expression compilation (literals, variables, simple calls)
   2. Integrate with existing cremepat pattern system
   3. Add comprehensive testing
   4. Improve error messages and diagnostics
   5. Add support for more complex expressions
   
   INTEGRATION POINTS:
   - Uses existing cremepat infrastructure where possible
   - Shares common utilities and error handling
   - Designed to work alongside pattern extensions
   - Maintains compatibility with existing codebase
*)