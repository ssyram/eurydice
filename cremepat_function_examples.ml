(*
   Example usage of cremepat function definitions in Eurydice context
   
   This demonstrates how built-in functions can now be written using
   the human-readable Rust-like syntax instead of complex pattern matching.
   
   Before: Complex nested pattern matching
   After: Clean, readable function definitions
*)

open Printf

(* Example showing how this would be used in practice *)
let example_builtin_function_patterns expr =
  match expr with
  
  (* NEW: Array to slice conversion using readable syntax *)
  | [%cremepat {|
      fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
        SliceRef<T> { pointer = arr, length = N }
      }
    |}] ->
      printf "Matched array_to_slice builtin function!\n";
      (* This would generate appropriate Krml code *)
      "Generated: Eurydice.array_to_slice implementation"

  (* NEW: Array copy function with bounds checking *)
  | [%cremepat {|
      fn array_copy<T><N : size_t>(src: &[T; N], dst: &[T; N]) -> () {
        {
          let i = 0;
          while i < N {
            dst[i] = src[i];
            i = i + 1
          }
        }
      }
    |}] ->
      printf "Matched array_copy builtin function!\n";
      "Generated: Eurydice.array_copy implementation"

  (* NEW: Simple arithmetic operations *)
  | [%cremepat {|
      fn add_u32(a: u32, b: u32) -> u32 {
        a + b
      }
    |}] ->
      printf "Matched add_u32 builtin function!\n";
      "Generated: Simple addition implementation"

  (* NEW: Generic comparison function *)
  | [%cremepat {|
      fn compare<T>(a: &T, b: &T) -> bool {
        a == b
      }
    |}] ->
      printf "Matched generic compare function!\n"; 
      "Generated: Generic comparison implementation"

  (* Existing cremepat patterns still work unchanged *)
  | [%cremepat {|
      let iter =
        core::iter::traits::collect::?::into_iter<
          core::iter::adapters::step_by::StepBy<core::ops::range::Range<?..>>,
          ?..
        >(core::iter::range::?::step_by<?..>(
          { start: ?e_start, end: ?e_end },
          ?e_increment
        ));
      ?rest..
    |}] ->
      printf "Matched existing iterator pattern (unchanged)!\n";
      "Generated: Iterator implementation"

  | _ ->
      printf "No pattern matched\n";
      "No implementation generated"

(* Demonstrate the benefits of this approach *)
let benefits_demo () =
  printf "\n=== BENEFITS OF CREMEPAT FUNCTION SUPPORT ===\n\n";
  
  printf "BEFORE (complex pattern matching):\n";
  printf "  • Hard to read nested constructor patterns\n";
  printf "  • Difficult to understand function signatures\n";
  printf "  • Error-prone manual AST construction\n";
  printf "  • Verbose and not maintainable\n\n";
  
  printf "AFTER (readable function syntax):\n";
  printf "  • Clear function signatures with types\n";
  printf "  • Familiar Rust-like syntax\n"; 
  printf "  • Easy to write and maintain\n";
  printf "  • Self-documenting code\n";
  printf "  • Type parameters clearly visible\n";
  printf "  • Const generics explicitly supported\n\n";

  printf "EXAMPLE TRANSFORMATION:\n";
  printf "Old style: ppat_cons_many ~loc \"EApp\" [...complex nesting...]\n";
  printf "New style: fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T>\n\n"

let () =
  printf "CREMEPAT FUNCTION DEFINITIONS - USAGE EXAMPLES\n";
  printf "===============================================\n\n";
  
  printf "This demonstrates how built-in functions can now be written\n";
  printf "using human-readable Rust-like syntax within cremepat blocks.\n\n";
  
  benefits_demo ();
  
  printf "The implementation provides:\n";
  printf "  ✓ Full type parameter support <T>\n";
  printf "  ✓ Const generic parameters <N : size_t>\n";
  printf "  ✓ Reference types &T and &[T; N]\n";
  printf "  ✓ Array types [T; N]\n";
  printf "  ✓ Return type annotations -> T\n";
  printf "  ✓ Function body expressions\n";
  printf "  ✓ Integration with existing cremepat patterns\n\n";
  
  printf "=== READY FOR PRODUCTION USE ===\n"