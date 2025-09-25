(* Demonstration of how cremepat_fun_def would be used in practice *)

open Printf

let demo_traditional_approach () =
  printf "=== Traditional DFunction Construction ===\n\n";
  printf "Currently, builtin functions in Eurydice are defined like this:\n\n";
  printf {|let array_to_slice = 
  let t = TBound 0 in
  fun lid ->
    DFunction (
      None,                              (* comment *)
      [ Private ],                       (* flags *)
      0,                                 (* n_cgs - const generics *)
      1,                                 (* n - type parameters *)
      TBuf (t, false),                   (* return type *)
      lid,                               (* location identifier *)
      [ Krml.Helpers.fresh_binder "arr" (TArray (t, _)) ], (* parameters *)
      (* Complex body construction with manual AST building *)
      with_type (TBuf (t, false)) 
        (EFlat [
          (Some "pointer", EAddrOf (EBound 0));
          (Some "length", EConstant (SizeT, "N"))
        ])
    )
|};
  printf "\n\nThis approach is:\n";
  printf "- ❌ Verbose and hard to read\n";
  printf "- ❌ Error-prone due to manual DeBruijn indices\n";
  printf "- ❌ Difficult to maintain\n";
  printf "- ❌ Requires deep knowledge of Krml AST structure\n\n"

let demo_new_approach () =
  printf "=== New cremepat_fun_def Approach ===\n\n";
  printf "With the new extension, the same function can be written as:\n\n";
  printf "let array_to_slice = \n";
  printf "  [%%cremepat_fun_def {|\n";
  printf "fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {\n";
  printf "  SliceRef<T> { pointer = arr, length = N }\n";
  printf "} |}]\n\n";
  printf "This approach is:\n";
  printf "- ✅ Concise and readable\n";
  printf "- ✅ Uses familiar Rust-like syntax\n";
  printf "- ✅ Self-documenting\n";
  printf "- ✅ Less error-prone\n";
  printf "- ✅ Easier to maintain\n\n"

let demo_pattern_matching () =
  printf "=== Generated Pattern Structure ===\n\n";
  printf "The cremepat_fun_def extension generates a pattern that matches:\n\n";
  printf {|DFunction (
  _,                              (* comment - any *)
  _,                              (* flags - any *)
  1,                              (* n_cgs - exactly 1 const generic *)
  1,                              (* n - exactly 1 type parameter *)
  SliceRef<T>,                    (* return type pattern *)
  (_, "array_to_slice"),          (* lid - module path and name *)
  [_],                            (* parameters - simplified for demo *)
  (* Body pattern matching SliceRef construction *)
  EFlat [
    (Some "pointer", ?arr_pattern);
    (Some "length", ?n_pattern)
  ]
)
|};
  printf "\n"

let demo_usage_examples () =
  printf "=== More Usage Examples ===\n\n";
  
  printf "1. Simple function with no generics:\n";
  printf "[%%cremepat_fun_def {|\n";
  printf "fn simple_add(a: i32, b: i32) -> i32 {\n";
  printf "  a + b\n";
  printf "} |}]\n\n";
  
  printf "2. Function with type parameters only:\n";
  printf "[%%cremepat_fun_def {|\n";
  printf "fn identity<T>(x: T) -> T {\n";
  printf "  x\n";
  printf "} |}]\n\n";
  
  printf "3. Function with const generics only:\n";
  printf "[%%cremepat_fun_def {|\n";
  printf "fn create_array<N : size_t>(value: i32) -> [i32; N] {\n";
  printf "  [value; N]\n";
  printf "} |}]\n\n";
  
  printf "4. Complex function with both:\n";
  printf "[%%cremepat_fun_def {|\n";
  printf "fn slice_to_array<T><N : size_t>(slice: &[T]) -> [T; N] {\n";
  printf "  assert!(slice.len() == N);\n";
  printf "  slice.try_into().unwrap()\n";
  printf "} |}]\n\n"

let demo_integration () =
  printf "=== Integration with Existing Code ===\n\n";
  printf "The new extension integrates seamlessly with existing cremepat usage:\n\n";
  printf "match some_expression with\n";
  printf "| [%%cremepat {| let x = ?e1; ?e2 |}] -> \n";
  printf "    (* Handle let binding *)\n";
  printf "    handle_let e1 e2\n";
  printf "    \n";
  printf "(* New function definition patterns work alongside *)\n";
  printf "| [%%cremepat_fun_def {|\n";
  printf "fn some_builtin<T>(param: T) -> T {\n";
  printf "  param\n";
  printf "} |}] ->\n";
  printf "    (* Handle function definition *)\n";
  printf "    handle_function_def ()\n";
  printf "    \n";
  printf "| _ -> (* Other cases *)\n\n"

let main () =
  printf "Cremepat Function Definition Usage Demonstration\n";
  printf "================================================\n\n";
  
  demo_traditional_approach ();
  demo_new_approach ();
  demo_pattern_matching ();
  demo_usage_examples ();
  demo_integration ();
  
  printf "Summary:\n";
  printf "--------\n";
  printf "The cremepat_fun_def extension provides a human-readable way to define\n";  
  printf "builtin functions in Eurydice, making the codebase more maintainable\n";
  printf "and easier to understand for newcomers.\n\n";
  printf "✓ Implementation complete and ready for testing!\n"

let () = main ()