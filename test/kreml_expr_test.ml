(* Test file to demonstrate kreml_expr extension functionality *)

open Krml.Ast

(* Test function using kreml_expr extension *)
let test_simple_swap =
  let t = TBound 0 in
  let binders =
    [
      Krml.Helpers.fresh_binder "ptr" (TBuf (t, false));
      Krml.Helpers.fresh_binder "i" (TInt SizeT);
      Krml.Helpers.fresh_binder "j" (TInt SizeT);
    ]
  in
  DFunction
    ( None,
      [ Private ],
      0,
      1,
      TUnit,
      (["test"], "simple_swap"),
      binders,
      (* Using kreml_expr for readable AST construction *)
      with_type TUnit [%kreml_expr {|
        let temp = ptr[i];
        ptr[i] = ptr[j];
        ptr[j] = temp
      |}] )

(* Test function using kreml_expr for match expression *)  
let test_option_unwrap_or =
  let t = TBound 0 in
  let t_option = TApp ((["core"; "option"], "Option"), [t]) in
  let binders =
    [
      Krml.Helpers.fresh_binder "opt" t_option;
      Krml.Helpers.fresh_binder "default" t;
    ]
  in
  DFunction
    ( None,
      [ Private ],
      0,
      1,
      t,
      (["test"], "unwrap_or"),
      binders,
      (* Using kreml_expr for pattern matching *)
      with_type t [%kreml_expr {|
        match opt {
          Some value -> value,
          None -> default
        }
      |}] )

(* Test file containing the functions *)
let test_file = ("kreml_expr_test", [test_simple_swap; test_option_unwrap_or])

(* Function to run the test through the main pipeline *)
let run_test_pipeline () =
  let files = [test_file] in
  
  (* Print initial AST *)
  Printf.printf "=== Initial Krml AST ===\n";
  List.iter (fun (name, decls) ->
    Printf.printf "File: %s\n" name;
    List.iter (fun decl ->
      Printf.printf "%s\n" (Krml.PrintAst.print_decl decl)
    ) decls;
    Printf.printf "\n"
  ) files;
  
  (* Run through some of the main pipeline stages *)
  Printf.printf "=== Running through pipeline stages ===\n";
  
  (* Stage 1: Type checking *)
  let errors, files = Krml.Checker.check_everything ~warn:true files in
  if errors then begin
    Printf.printf "Type checking failed!\n";
    exit 1
  end else
    Printf.printf "✓ Type checking passed\n";
  
  (* Stage 2: Some basic optimizations *)
  let files = Krml.Simplify.optimize_lets files in
  let files = Krml.Simplify.remove_unused files in
  
  Printf.printf "=== Final Krml AST after optimizations ===\n";
  List.iter (fun (name, decls) ->
    Printf.printf "File: %s\n" name;
    List.iter (fun decl ->
      Printf.printf "%s\n" (Krml.PrintAst.print_decl decl)
    ) decls;
    Printf.printf "\n"
  ) files;
  
  Printf.printf "✅ Test pipeline completed successfully\n";
  files

let () =
  Printf.printf "Running kreml_expr extension test...\n\n";
  let _final_files = run_test_pipeline () in
  Printf.printf "Test completed!\n"