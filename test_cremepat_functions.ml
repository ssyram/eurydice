(* Test binary for cremepat function support 
   This file tests the new function definition syntax in cremepat
   
   To run this test:
   1. Build the project: make build
   2. Run this test: ocaml test_cremepat_functions.ml
   
   This test verifies that the function parsing works correctly
   by parsing example function definitions and printing the results.
*)

open Printf

(* Mock Krml AST types for testing - in real usage these would come from Krml *)
type test_pattern = 
  | TestPattern of string

(* Test function to parse a cremepat function definition *)
let test_function_parse input_str =
  printf "Testing cremepat function parsing:\n";
  printf "Input: %s\n" input_str;
  try
    (* This would normally use the cremepat parser *)
    let lexbuf = Sedlexing.Utf8.from_string input_str in
    let parser_func = MenhirLib.Convert.Simplified.traditional2revised Cremepat.Parse.fragment in
    let result = parser_func (fun _ -> Cremepat.Lex.token lexbuf) in
    printf "Parsed successfully!\n";
    (* Print the parsed AST structure *)
    match result with 
    | Fixed (FunctionDef { name; type_params; cg_params; params; return_type; body }) ->
        printf "Function name: %s\n" name;
        printf "Type parameters: [%s]\n" (String.concat "; " type_params);
        printf "Const generic params: %d\n" (List.length cg_params);
        printf "Parameters: %d\n" (List.length params);
        printf "Has return type: %b\n" (return_type <> None);
        printf "SUCCESS: Function definition parsed correctly!\n"
    | _ ->
        printf "ERROR: Not a function definition\n"
  with
  | exn ->
      printf "ERROR: %s\n" (Printexc.to_string exn)

(* Test cases *)
let () =
  printf "=== CREMEPAT FUNCTION SUPPORT TEST ===\n\n";
  
  (* Test case 1: Simple function without generics *)
  test_function_parse {|
    fn simple_func(x: i32) -> i32 {
      x
    }
  |};
  
  printf "\n";
  
  (* Test case 2: Function with type parameters *)
  test_function_parse {|
    fn generic_func<T>(x: T) -> T {
      x  
    }
  |};
  
  printf "\n";
  
  (* Test case 3: The main example from the requirements *)
  test_function_parse {|
    fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
      SliceRef<T> { pointer = arr, length = N }
    }
  |};
  
  printf "\n=== TEST COMPLETE ===\n"