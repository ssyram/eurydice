(* =================================================================================================
   CREMEPAT BUILTIN EXPRESSIONS: Minimal AST for Builtin Function Calls
   =================================================================================================
   
   This module provides a minimal AST focused exclusively on builtin function calls.
   By limiting scope to only builtins from Builtin.ml, we eliminate the need for
   complex type inference while providing immediate practical value.
   
   DESIGN PRINCIPLES:
   - Simple AST matching builtin function signatures exactly
   - Direct mapping to existing Builtin.ml definitions
   - Pattern variables for flexible code generation
   - No complex expressions, control flow, or type inference
 *)

(* =================================================================================================
   MINIMAL AST FOR BUILTIN CALLS
   ================================================================================================= *)

(* Builtin function call representation *)
type builtin_call = {
  name : string;                    (* Function name: "slice_index", "array_to_slice", etc. *)
  type_args : string list;          (* Type arguments: ["i32"], ["T", "U"], etc. *)
  args : builtin_arg list;          (* Function arguments *)
}

(* Arguments to builtin functions *)
and builtin_arg = 
  | PatternVar of string            (* ?x - captures existing expression *)
  | Literal of literal_value        (* Simple literals for testing *)

(* Simple literal support for testing and simple cases *)
and literal_value =
  | IntLit of int                   (* 42 *)
  | BoolLit of bool                 (* true, false *)
  | StringLit of string             (* "hello" (rarely used in builtins) *)

(* =================================================================================================
   BUILTIN FUNCTION REGISTRY
   ================================================================================================= *)

(* Builtin function information extracted from Builtin.ml *)
type builtin_info = {
  krml_name : string list * string; (* Qualified name for Krml *)
  arg_count : int;                  (* Expected number of arguments *)
  type_arg_count : int;             (* Expected number of type arguments *)
  description : string;             (* Human-readable description *)
}

(* Registry of all supported builtin functions *)
let builtin_registry = [
  (* Array operations *)
  "array_to_slice", {
    krml_name = (["Eurydice"], "array_to_slice");
    arg_count = 1;
    type_arg_count = 1;
    description = "Convert array to slice";
  };
  
  "array_to_subslice", {
    krml_name = (["Eurydice"], "array_to_subslice");
    arg_count = 2;
    type_arg_count = 3;
    description = "Convert array to subslice with range";
  };
  
  (* Slice operations *)
  "slice_index", {
    krml_name = (["Eurydice"], "slice_index");
    arg_count = 2;
    type_arg_count = 1;
    description = "Index into slice";
  };
  
  "slice_subslice", {
    krml_name = (["Eurydice"], "slice_subslice");
    arg_count = 2;
    type_arg_count = 3;
    description = "Create subslice from slice";
  };
  
  (* Discriminant operation *)
  "discriminant", {
    krml_name = (["Eurydice"], "discriminant");
    arg_count = 1;
    type_arg_count = 2;
    description = "Get discriminant of ADT";
  };
  
  (* Vector operations *)
  "vec_new", {
    krml_name = (["Eurydice"], "vec_new");
    arg_count = 0;
    type_arg_count = 1;
    description = "Create new empty vector";
  };
  
  "vec_push", {
    krml_name = (["Eurydice"], "vec_push");
    arg_count = 2;
    type_arg_count = 1;
    description = "Push element to vector";
  };
  
  (* Range operations *)
  "range_iterator_next", {
    krml_name = (["Eurydice"], "range_iterator_next");
    arg_count = 1;
    type_arg_count = 1;
    description = "Get next element from range iterator";
  };
  
  (* 128-bit integer operations (simplified selection) *)
  "i128_add", {
    krml_name = (["Eurydice"; "Int128"], "i_add");
    arg_count = 2;
    type_arg_count = 0;
    description = "Add two i128 values";
  };
  
  "u128_add", {
    krml_name = (["Eurydice"; "Int128"], "u_add");
    arg_count = 2;
    type_arg_count = 0;
    description = "Add two u128 values";
  };
]

(* Convert registry to map for fast lookup *)
let builtin_map = 
  List.fold_left (fun acc (name, info) -> 
    Hashtbl.add acc name info; acc
  ) (Hashtbl.create 32) builtin_registry

(* =================================================================================================
   VALIDATION AND LOOKUP FUNCTIONS
   ================================================================================================= *)

(* Look up builtin function info *)
let lookup_builtin name =
  try Some (Hashtbl.find builtin_map name)
  with Not_found -> None

(* Validate builtin call structure *)
let validate_builtin_call call =
  match lookup_builtin call.name with
  | None -> 
      Error (Printf.sprintf "Unknown builtin function: %s" call.name)
  | Some info ->
      (* Check argument count *)
      if List.length call.args <> info.arg_count then
        Error (Printf.sprintf "Function %s expects %d arguments, got %d" 
          call.name info.arg_count (List.length call.args))
      (* Check type argument count *)
      else if List.length call.type_args <> info.type_arg_count then
        Error (Printf.sprintf "Function %s expects %d type arguments, got %d"
          call.name info.type_arg_count (List.length call.type_args))
      else
        Ok info

(* =================================================================================================
   UTILITY FUNCTIONS
   ================================================================================================= *)

(* Create builtin call *)
let make_builtin_call name ?(type_args = []) args = {
  name;
  type_args;
  args;
}

(* Create pattern variable argument *)
let pattern_var name = PatternVar name

(* Create literal argument *)
let int_literal n = Literal (IntLit n)
let bool_literal b = Literal (BoolLit b)
let string_literal s = Literal (StringLit s)

(* Get all available builtin function names *)
let available_builtins () = 
  List.map fst builtin_registry

(* Pretty print builtin call for debugging *)
let string_of_builtin_call call =
  let type_args_str = 
    if call.type_args = [] then ""
    else "::<" ^ String.concat ", " call.type_args ^ ">"
  in
  let args_str = 
    String.concat ", " (List.map (function
      | PatternVar name -> "?" ^ name
      | Literal (IntLit n) -> string_of_int n
      | Literal (BoolLit b) -> string_of_bool b
      | Literal (StringLit s) -> "\"" ^ s ^ "\""
    ) call.args)
  in
  Printf.sprintf "%s%s(%s)" call.name type_args_str args_str

(* =================================================================================================
   EXAMPLES AND TESTING UTILITIES
   ================================================================================================= *)

(* Example builtin calls for testing *)
let example_calls = [
  make_builtin_call "slice_index" ~type_args:["i32"] [pattern_var "slice"; pattern_var "index"];
  make_builtin_call "array_to_slice" ~type_args:["u8"] [pattern_var "array"];
  make_builtin_call "discriminant" ~type_args:["MyEnum"; "u32"] [pattern_var "value"];
  make_builtin_call "vec_new" ~type_args:["i32"] [];
  make_builtin_call "i128_add" [pattern_var "a"; pattern_var "b"];
]

(* Test validation of example calls *)
let test_validation () =
  Printf.printf "Testing builtin call validation:\n";
  List.iteri (fun i call ->
    Printf.printf "%d. %s -> " (i+1) (string_of_builtin_call call);
    match validate_builtin_call call with
    | Ok info -> Printf.printf "OK (%s)\n" info.description
    | Error msg -> Printf.printf "ERROR: %s\n" msg
  ) example_calls

(* =================================================================================================
   INTEGRATION POINTS
   ================================================================================================= *)

(* This module provides the foundation for:
   1. BuiltinParser.ml - parsing builtin call syntax
   2. BuiltinExtension.ml - PPX integration and Krml generation
   
   The design is intentionally minimal to keep implementation simple
   while providing immediate practical value for builtin function usage.
   
   Future extensions can add:
   - More builtin functions from Builtin.ml
   - Simple variable references
   - Basic composition of builtin calls
   - Integration with existing cremepat patterns
*)