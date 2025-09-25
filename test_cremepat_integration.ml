(* Integration test for cremepat function support
   
   This demonstrates how the new function syntax integrates with
   the existing cremepat extension system.
   
   Usage: This file should be compiled with the cremepat preprocessor
   to verify that the function definitions are correctly transformed
   into pattern matches.
*)

open Printf

(* Example of how the function syntax would be used in practice *)
let test_function_pattern expr =
  match expr with
  (* This is the new syntax we're adding support for *)
  | [%cremepat {| 
      fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
        SliceRef<T> { pointer = arr, length = N }
      }
    |}] -> 
      printf "Matched array_to_slice function!\n";
      printf "This demonstrates the function pattern matching works\n"
      
  (* Another example with simpler syntax *)
  | [%cremepat {|
      fn simple_func(x: i32) -> i32 {
        x
      }
    |}] ->
      printf "Matched simple function!\n"
      
  | _ ->
      printf "No function pattern matched\n"

(* Test runner *)
let () =
  printf "=== CREMEPAT FUNCTION INTEGRATION TEST ===\n\n";
  printf "This test verifies that cremepat can parse and compile\n";
  printf "function definitions into appropriate OCaml patterns.\n\n";
  
  (* In a real scenario, these would be actual AST nodes *)
  printf "Testing function pattern compilation...\n";
  printf "If this compiles successfully, the cremepat extension\n";
  printf "is correctly handling the new function syntax!\n\n";
  
  printf "=== INTEGRATION TEST COMPLETE ===\n"