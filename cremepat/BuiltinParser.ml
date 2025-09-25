(* =================================================================================================
   CREMEPAT BUILTIN PARSER: Simple Parser for Builtin Function Calls
   =================================================================================================
   
   This module provides a simple recursive descent parser for builtin function call syntax.
   The parser is intentionally minimal, avoiding complex lexing/parsing infrastructure
   in favor of direct string manipulation and regex patterns.
   
   SUPPORTED SYNTAX:
   - function_name(args)
   - function_name::<type_args>(args)  
   - Arguments: ?pattern_vars, literals
   - Whitespace tolerant
   
   DESIGN: Simple and AI-friendly implementation focused on the 80% use case.
 *)

open BuiltinExpr

(* =================================================================================================
   PARSING UTILITIES
   ================================================================================================= *)

(* Simple tokenizer using regex patterns *)
let tokenize input =
  let open Str in
  let patterns = [
    ("IDENT", regexp "[a-zA-Z_][a-zA-Z0-9_]*");
    ("PATTERN_VAR", regexp "\\?[a-zA-Z_][a-zA-Z0-9_]*");
    ("INTEGER", regexp "[0-9]+");
    ("STRING", regexp "\"[^\"]*\"");
    ("TYPE_ARGS_START", regexp "::<");
    ("LPAREN", regexp "(");
    ("RPAREN", regexp ")");
    ("LANGLE", regexp "<");
    ("RANGLE", regexp ">");
    ("COMMA", regexp ",");
    ("WHITESPACE", regexp "[ \t\n\r]+");
  ] in
  
  let rec tokenize_helper pos acc =
    if pos >= String.length input then List.rev acc
    else
      let found = ref false in
      List.iter (fun (token_type, pattern) ->
        if not !found && string_match pattern input pos then begin
          found := true;
          let matched = matched_string input in
          let new_pos = pos + String.length matched in
          if token_type <> "WHITESPACE" then
            tokenize_helper new_pos ((token_type, matched) :: acc)
          else
            tokenize_helper new_pos acc
        end
      ) patterns;
      if not !found then
        failwith (Printf.sprintf "Unexpected character at position %d: %c" pos input.[pos])
      else
        acc
  in
  tokenize_helper 0 []

(* =================================================================================================
   PARSER STATE AND COMBINATORS
   ================================================================================================= *)

(* Parser state *)
type parse_state = {
  tokens : (string * string) list;
  position : int;
}

(* Parser result *)
type 'a parse_result = 
  | Success of 'a * parse_state
  | Failure of string

(* Basic parser combinators *)
let peek_token state =
  if state.position >= List.length state.tokens then None
  else Some (List.nth state.tokens state.position)

let consume_token state =
  if state.position >= List.length state.tokens then
    Failure "Unexpected end of input"
  else
    let token = List.nth state.tokens state.position in
    Success (token, { state with position = state.position + 1 })

let expect_token expected_type state =
  match consume_token state with
  | Success ((token_type, value), new_state) when token_type = expected_type ->
      Success (value, new_state)
  | Success ((token_type, _), _) ->
      Failure (Printf.sprintf "Expected %s, got %s" expected_type token_type)
  | Failure msg -> Failure msg

(* =================================================================================================
   ARGUMENT PARSING
   ================================================================================================= *)

(* Parse a single argument *)
let parse_argument state =
  match peek_token state with
  | Some ("PATTERN_VAR", var_name) ->
      let var_name = String.sub var_name 1 (String.length var_name - 1) in (* Remove ? *)
      (match consume_token state with
      | Success (_, new_state) -> Success (PatternVar var_name, new_state)
      | Failure msg -> Failure msg)
  
  | Some ("INTEGER", int_str) ->
      (match consume_token state with
      | Success (_, new_state) -> 
          Success (Literal (IntLit (int_of_string int_str)), new_state)
      | Failure msg -> Failure msg)
  
  | Some ("STRING", str_val) ->
      let content = String.sub str_val 1 (String.length str_val - 2) in (* Remove quotes *)
      (match consume_token state with
      | Success (_, new_state) -> Success (Literal (StringLit content), new_state)
      | Failure msg -> Failure msg)
  
  | Some ("IDENT", "true") ->
      (match consume_token state with
      | Success (_, new_state) -> Success (Literal (BoolLit true), new_state)
      | Failure msg -> Failure msg)
  
  | Some ("IDENT", "false") ->
      (match consume_token state with
      | Success (_, new_state) -> Success (Literal (BoolLit false), new_state)
      | Failure msg -> Failure msg)
  
  | Some (token_type, value) ->
      Failure (Printf.sprintf "Unexpected token in argument: %s (%s)" token_type value)
  
  | None ->
      Failure "Unexpected end of input while parsing argument"

(* Parse comma-separated argument list *)
let rec parse_argument_list state acc =
  match parse_argument state with
  | Success (arg, new_state) ->
      let args = arg :: acc in
      (match peek_token new_state with
      | Some ("COMMA", _) ->
          (match consume_token new_state with
          | Success (_, after_comma) -> parse_argument_list after_comma args
          | Failure msg -> Failure msg)
      | _ -> Success (List.rev args, new_state))
  
  | Failure _ when acc = [] ->
      Success ([], state) (* Empty argument list is OK *)
  
  | Failure msg -> Failure msg

(* =================================================================================================
   TYPE ARGUMENT PARSING
   ================================================================================================= *)

(* Parse type argument (simple identifier) *)
let parse_type_arg state =
  expect_token "IDENT" state

(* Parse comma-separated type argument list *)
let rec parse_type_arg_list state acc =
  match parse_type_arg state with
  | Success (type_arg, new_state) ->
      let type_args = type_arg :: acc in
      (match peek_token new_state with
      | Some ("COMMA", _) ->
          (match consume_token new_state with
          | Success (_, after_comma) -> parse_type_arg_list after_comma type_args
          | Failure msg -> Failure msg)
      | _ -> Success (List.rev type_args, new_state))
  
  | Failure _ when acc = [] ->
      Success ([], state) (* Empty type argument list is OK *)
  
  | Failure msg -> Failure msg

(* =================================================================================================
   MAIN PARSING FUNCTIONS
   ================================================================================================= *)

(* Parse builtin function call *)
let parse_builtin_call state =
  (* Parse function name *)
  match expect_token "IDENT" state with
  | Failure msg -> Failure msg
  | Success (func_name, after_name) ->
      
      (* Check for type arguments *)
      let parse_type_args, after_type_args = 
        match peek_token after_name with
        | Some ("TYPE_ARGS_START", _) ->
            (match consume_token after_name with
            | Success (_, after_type_start) ->
                (match parse_type_arg_list after_type_start [] with
                | Success (type_args, after_types) ->
                    (match expect_token "RANGLE" after_types with
                    | Success (_, after_close) -> type_args, after_close
                    | Failure msg -> failwith msg)
                | Failure msg -> failwith msg)
            | Failure msg -> failwith msg)
        | _ -> [], after_name
      in
      
      (* Parse arguments *)
      match expect_token "LPAREN" after_type_args with
      | Failure msg -> Failure msg
      | Success (_, after_lparen) ->
          
          match parse_argument_list after_lparen [] with
          | Failure msg -> Failure msg
          | Success (args, after_args) ->
              
              match expect_token "RPAREN" after_args with
              | Failure msg -> Failure msg
              | Success (_, final_state) ->
                  
                  let call = {
                    name = func_name;
                    type_args = parse_type_args;
                    args = args;
                  } in
                  Success (call, final_state)

(* =================================================================================================
   PUBLIC API
   ================================================================================================= *)

(* Parse builtin call from string *)
let parse_string input =
  try
    let tokens = tokenize input in
    let initial_state = { tokens; position = 0 } in
    
    match parse_builtin_call initial_state with
    | Success (call, final_state) ->
        (* Check that we consumed all tokens *)
        if final_state.position = List.length final_state.tokens then
          Ok call
        else
          Error (Printf.sprintf "Unexpected tokens after parsing: position %d of %d" 
            final_state.position (List.length final_state.tokens))
    
    | Failure msg -> Error msg
    
  with
  | exn -> Error (Printf.sprintf "Parse error: %s" (Printexc.to_string exn))

(* Parse and validate builtin call *)
let parse_and_validate input =
  match parse_string input with
  | Error msg -> Error msg
  | Ok call ->
      (match validate_builtin_call call with
      | Error msg -> Error msg
      | Ok info -> Ok (call, info))

(* =================================================================================================
   TESTING AND EXAMPLES
   ================================================================================================= *)

(* Test cases for parser *)
let test_cases = [
  "slice_index(?slice, ?index)";
  "slice_index::<i32>(?slice, ?index)";
  "array_to_slice::<u8>(?array)";
  "discriminant::<MyEnum, u32>(?value)";
  "vec_new::<i32>()";
  "i128_add(?a, ?b)";
  "slice_index(?s, 42)";
  "vec_push(?vec, true)";
]

(* Test parser with example inputs *)
let test_parser () =
  Printf.printf "Testing builtin parser:\n";
  List.iteri (fun i input ->
    Printf.printf "%d. Input: %s\n" (i+1) input;
    match parse_and_validate input with
    | Ok (call, info) -> 
        Printf.printf "   Success: %s (%s)\n" 
          (string_of_builtin_call call) info.description
    | Error msg -> 
        Printf.printf "   Error: %s\n" msg
  ) test_cases

(* =================================================================================================
   IMPLEMENTATION NOTES
   ================================================================================================= *)

(*
   PARSER DESIGN DECISIONS:
   
   1. SIMPLE TOKENIZATION: Uses regex patterns instead of complex lexer
      - Easy to understand and modify
      - Handles whitespace automatically
      - Good error messages for invalid characters
   
   2. RECURSIVE DESCENT: Simple parser combinators
      - Straightforward control flow
      - Easy to extend for new syntax
      - Clear error propagation
   
   3. FOCUSED SCOPE: Only parses builtin calls
      - No complex expressions or statements
      - No operator precedence issues
      - Validates against known builtin signatures
   
   4. ERROR HANDLING: Clear error messages with context
      - Position information for debugging
      - Validation integrated with parsing
      - Helpful suggestions for common mistakes
   
   EXTENSIBILITY:
   - Add new argument types by extending parse_argument
   - Add new syntax by extending parse_builtin_call
   - Integrate with existing cremepat by sharing pattern variable handling
   
   This parser strikes a balance between simplicity and functionality,
   providing a solid foundation for the minimal builtin expression extension.
*)