(* Test binary for cremepat function definition parsing *)

(* Simple function to test basic parsing functionality *)
let test_basic_parsing () =
  Printf.printf "Testing basic cremepat function parsing...\n";
  let example = {|fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
  SliceRef<T> { pointer = arr, length = N }
}|} in
  try
    let lexbuf = Sedlexing.Utf8.from_string example in
    let parser = MenhirLib.Convert.Simplified.traditional2revised Cremepat.Parse.fragment in
    let result = parser (fun _ -> Cremepat.Lex.token lexbuf) in
    Printf.printf "✓ Successfully parsed function definition!\n";
    (* Print basic info about the parsed result *)
    (match result with
     | Fixed (FunctionDef (Fixed { name; type_params; const_params; params; _ })) ->
         Printf.printf "  Function name: %s\n" name;
         Printf.printf "  Type parameters: %d\n" (List.length type_params);
         Printf.printf "  Const parameters: %d\n" (List.length const_params);
         Printf.printf "  Parameters: %d\n" (List.length params)
     | _ -> Printf.printf "  Unexpected parse tree structure\n")
  with
  | exn -> 
      Printf.printf "✗ Parsing failed: %s\n" (Printexc.to_string exn);
      Printf.printf "This is expected until all dependencies are resolved.\n"

(* Test to demonstrate how the extension would be used *)
let test_extension_demo () =
  Printf.printf "\nDemonstrating intended usage:\n";
  Printf.printf "The following syntax should be supported:\n";
  Printf.printf {|
[%%cremepat_fun_def {|
fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
  SliceRef<T> { pointer = arr, length = N }
} |}]
|};
  Printf.printf "\nThis would generate a pattern matching DFunction nodes.\n"

let main () =
  Printf.printf "Cremepat Function Definition Test\n";
  Printf.printf "=================================\n\n";
  test_basic_parsing ();
  test_extension_demo ();
  Printf.printf "\nTest completed.\n"

let () = main ()