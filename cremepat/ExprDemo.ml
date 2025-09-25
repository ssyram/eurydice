(* =================================================================================================
   CREMEPAT EXPRESSION DEMO: Example Usage and Testing
   =================================================================================================
   
   This file demonstrates the intended usage of the cremepat expression extension
   and provides examples for testing the implementation.
   
   NOTE: This is demonstration code - the actual PPX extension is not yet complete.
   These examples show the intended syntax and behavior.
 *)

(* =================================================================================================
   USAGE EXAMPLES
   ================================================================================================= *)

(* Basic literals and variables *)
let example_literals () =
  (* These would expand to Krml.Ast expressions *)
  let int_expr = [%cremeexpr {| 42 |}] in
  let bool_expr = [%cremeexpr {| true |}] in
  let string_expr = [%cremeexpr {| "hello" |}] in
  let unit_expr = [%cremeexpr {| () |}] in
  
  Printf.printf "Basic literals compiled successfully\n";
  (int_expr, bool_expr, string_expr, unit_expr)

(* Function calls with generics *)
let example_function_calls () =
  (* Simple function call *)
  let simple_call = [%cremeexpr {| f(a, b, c) |}] in
  
  (* Generic instantiation *)
  let generic_call = [%cremeexpr {| Vec::<i32>::new() |}] in
  
  (* Complex generics *)
  let complex_generic = [%cremeexpr {| HashMap::<String, Vec<i32>>::with_capacity(10) |}] in
  
  Printf.printf "Function calls compiled successfully\n";
  (simple_call, generic_call, complex_generic)

(* Mixed concrete and pattern expressions *)
let example_pattern_integration () =
  (* Pattern variables in expressions *)
  let with_pattern_var = [%cremeexpr {| Vec::<i32>::with_capacity(?size) |}] in
  
  (* Mixed concrete and pattern *)
  let mixed_expr = [%cremeexpr {| let x = ?init; x + 42 |}] in
  
  (* List pattern variables *)
  let with_list_pattern = [%cremeexpr {| f(?first_arg, ?rest_args..) |}] in
  
  Printf.printf "Pattern integration compiled successfully\n";
  (with_pattern_var, mixed_expr, with_list_pattern)

(* Control flow expressions *)
let example_control_flow () =
  (* If expressions *)
  let if_expr = [%cremeexpr {| if cond { 42 } else { 24 } |}] in
  
  (* Let bindings *)
  let let_expr = [%cremeexpr {| let x = 42; let y = x + 1; y * 2 |}] in
  
  (* While loops *)
  let while_expr = [%cremeexpr {| while i < 10 { i += 1 } |}] in
  
  Printf.printf "Control flow compiled successfully\n";
  (if_expr, let_expr, while_expr)

(* Type annotations *)
let example_type_annotations () =
  (* Explicit type annotations *)
  let typed_let = [%cremeexpr {| let x : i32 = 42; x + 1 |}] in
  
  (* Type ascription *)
  let type_ascription = [%cremeexpr {| (Vec::new() : Vec<i32>) |}] in
  
  (* Typed integers *)
  let typed_int = [%cremeexpr {| 42u64 + 100u64 |}] in
  
  Printf.printf "Type annotations compiled successfully\n";
  (typed_let, type_ascription, typed_int)

(* =================================================================================================
   TESTING UTILITIES (for development)
   ================================================================================================= *)

(* Test parsing of expression strings *)
let test_parsing () =
  let test_cases = [
    "42";
    "true";
    "x";
    "f(a, b)";
    "Vec::<i32>::new()";
    "let x = 42; x + 1";
    "if cond { 42 } else { 24 }";
    "Vec::<i32>::with_capacity(?size)";
  ] in
  
  Printf.printf "Testing expression parsing...\n";
  List.iter (fun expr_str ->
    Printf.printf "  Testing: %s\n" expr_str;
    (* Would call ExprExtension.debug_compile_expression expr_str *)
  ) test_cases;
  Printf.printf "Parsing tests complete\n"

(* Benchmark expression generation vs manual construction *)
let benchmark_performance () =
  Printf.printf "Benchmarking expression generation...\n";
  
  (* Manual Krml expression construction *)
  let manual_vec_new () =
    let open Krml.Ast in
    with_type (TApp (TQualified (["std"; "vec"], "Vec"), [TInt I32]))
      (EApp (
        with_type (TArrow ([], TApp (TQualified (["std"; "vec"], "Vec"), [TInt I32])))
          (ETApp (
            with_type (TScheme (["T"], TArrow ([], TApp (TQualified (["std"; "vec"], "Vec"), [TBound 0]))))
              (EQualified (["std"; "vec"; "Vec"], "new")),
            [], [], [TInt I32])),
        []))
  in
  
  (* Cremepat version (would be) *)
  let cremepat_vec_new () =
    [%cremeexpr {| Vec::<i32>::new() |}]
  in
  
  Printf.printf "Manual construction: complex nested structure\n";
  Printf.printf "Cremepat version: simple readable syntax\n";
  Printf.printf "Benchmark complete\n"

(* =================================================================================================
   INTEGRATION EXAMPLES
   ================================================================================================= *)

(* Example of using cremepat expressions in real code *)
let example_real_usage () =
  (* Before: Manual Krml expression construction *)
  let build_range_loop_manual start end_ body =
    let open Krml.Ast in
    with_type TUnit (EWhile (
      with_type TBool (EApp (
        with_type (TArrow ([TInt I32; TInt I32], TBool)) (EQualified ([], "<")),
        [with_type (TInt I32) (EBound 0); end_])),
      with_type TUnit (ESequence [
        body;
        with_type TUnit (EAssign (
          with_type (TInt I32) (EBound 0),
          with_type (TInt I32) (EApp (
            with_type (TArrow ([TInt I32; TInt I32], TInt I32)) (EQualified ([], "+")),
            [with_type (TInt I32) (EBound 0); 
             with_type (TInt I32) (EConstant (Int (I32, "1")))]))))]))
  in
  
  (* After: Using cremepat expressions *)
  let build_range_loop_cremepat start end_ body =
    [%cremeexpr {|
      while i < {expr end_} {
        {expr body};
        i = i + 1
      }
    |}]
  in
  
  Printf.printf "Integration example shows significant simplification\n"

(* =================================================================================================
   MAIN DEMO FUNCTION
   ================================================================================================= *)

let run_demo () =
  Printf.printf "=== Cremepat Expression Extension Demo ===\n\n";
  
  Printf.printf "1. Basic Examples:\n";
  test_parsing ();
  Printf.printf "\n";
  
  Printf.printf "2. Performance Comparison:\n";
  benchmark_performance ();
  Printf.printf "\n";
  
  Printf.printf "3. Real-world Integration:\n";
  example_real_usage ();
  Printf.printf "\n";
  
  Printf.printf "=== Demo Complete ===\n";
  
  (* Note: Actual compilation examples would only work once PPX is complete *)
  Printf.printf "\nNote: PPX extension not yet complete - examples show intended usage\n"

(* Uncomment to run demo when module is loaded *)
(* let () = run_demo () *)