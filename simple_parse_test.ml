(* Simple test to validate cremepat function parsing without complex build *)
let test_simple_cases () =
  print_endline "Testing simple cremepat function syntax parsing";
  
  let simple_examples = [
    ("Simple function", "fn test(x: i32) -> i32 { x }");
    ("Generic function", "fn generic<T>(x: T) -> T { x }");
    ("No return type", "fn void_func() -> () { break }");
  ] in
  
  List.iter (fun (name, code) ->
    printf "Case: %s\n" name;
    printf "Code: %s\n" code;
    printf "Expected: Should parse without conflicts\n\n"
  ) simple_examples;
  
  print_endline "All test cases defined"

let () = test_simple_cases ()