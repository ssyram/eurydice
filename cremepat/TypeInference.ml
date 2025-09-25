(* =================================================================================================
   CREMEPAT TYPE INFERENCE ENGINE
   =================================================================================================
   
   This module implements type inference for cremepat expressions.
   The goal is to balance inference power with implementation simplicity,
   providing good error messages when inference fails.
   
   DESIGN PHILOSOPHY:
   - Local inference with explicit annotations as fallback
   - Bottom-up inference from literals and variables
   - Top-down constraint propagation from context
   - Clear error messages with suggestions
 *)

open ExprParseTree

(* =================================================================================================
   TYPE INFERENCE CONTEXT AND STATE
   ================================================================================================= *)

(* Inference context - tracks state during type checking *)
type inference_ctx = {
  env : type_env;                          (* current type environment *)
  constraints : unification_constraint list;  (* accumulated constraints *)
  type_var_counter : int ref;              (* for generating fresh type variables *)
  errors : (location option * string) list; (* accumulated errors *)
}

(* Create fresh type variable *)
let fresh_type_var ctx =
  let n = !(ctx.type_var_counter) in
  ctx.type_var_counter := n + 1;
  TVar ("$t" ^ string_of_int n)

(* =================================================================================================
   CONSTRAINT GENERATION AND UNIFICATION
   ================================================================================================= *)

(* Add unification constraint *)
let add_constraint ctx left right reason =
  let constraint_ = { left; right; reason } in
  { ctx with constraints = constraint_ :: ctx.constraints }

(* Add error to context *)
let add_error ctx location message =
  { ctx with errors = (location, message) :: ctx.errors }

(* Check if two types can be unified *)
let rec can_unify typ1 typ2 =
  match typ1.typ, typ2.typ with
  | TVar _, _ | _, TVar _ -> true                    (* type variables unify with anything *)
  | TInt w1, TInt w2 -> w1 = w2                     (* same integer width *)
  | TBool, TBool | TStr, TStr | TUnit, TUnit -> true (* identical base types *)
  | TQualified p1, TQualified p2 -> p1 = p2         (* same qualified name *)
  | TApp (base1, args1), TApp (base2, args2) ->
      can_unify { typ = base1; inferred = true; source_loc = None } 
                { typ = base2; inferred = true; source_loc = None } &&
      List.length args1 = List.length args2 &&
      List.for_all2 can_unify
        (List.map (fun t -> { typ = t; inferred = true; source_loc = None }) args1)
        (List.map (fun t -> { typ = t; inferred = true; source_loc = None }) args2)
  | _ -> false

(* Placeholder for full unification algorithm *)
let unify_types ctx typ1 typ2 reason =
  if can_unify typ1 typ2 then
    ctx  (* Success - would need to apply substitutions in full implementation *)
  else
    add_error ctx typ1.source_loc 
      (Printf.sprintf "Type mismatch: cannot unify %s with %s (%s)"
         (string_of_type typ1.typ) (string_of_type typ2.typ) reason)

(* Convert type to string for error messages *)
and string_of_type = function
  | TInt w -> "i32"  (* simplified - would need full width handling *)
  | TBool -> "bool"
  | TStr -> "str"
  | TUnit -> "()"
  | TVar name -> name
  | TQualified (path, name) -> String.concat "::" (path @ [name])
  | TApp (base, args) -> 
      string_of_type base ^ "<" ^ 
      String.concat ", " (List.map string_of_type args) ^ ">"

(* =================================================================================================
   LITERAL TYPE INFERENCE
   ================================================================================================= *)

(* Infer type of literals - always succeeds *)
let infer_literal_type = function
  | LInt (_, Some width) -> make_typ_info (TInt width) ~inferred:false ()
  | LInt (_, None) -> make_typ_info (TInt I32) ~inferred:true ()  (* default to i32 *)
  | LBool _ -> make_typ_info TBool ~inferred:true ()
  | LStr _ -> make_typ_info TStr ~inferred:true ()
  | LUnit -> make_typ_info TUnit ~inferred:true ()

(* =================================================================================================
   VARIABLE AND QUALIFIED NAME RESOLUTION
   ================================================================================================= *)

(* Look up variable in environment *)
let lookup_variable ctx name location =
  match lookup_var ctx.env name with
  | Some binding -> 
      (ctx, Success binding.typ)
  | None ->
      let error_msg = Printf.sprintf "Unbound variable: %s" name in
      let ctx' = add_error ctx location error_msg in
      (ctx', TypeError error_msg)

(* Look up qualified name (function, type constructor, etc.) *)
let lookup_qualified_name ctx path location =
  (* Placeholder implementation - would need integration with target type system *)
  match List.assoc_opt path ctx.env.functions with
  | Some signature ->
      (* Create fresh type variables for generic parameters *)
      let fresh_vars = List.map (fun _ -> fresh_type_var ctx) signature.type_params in
      (* TODO: Substitute type parameters with fresh variables *)
      (ctx, Success signature.return_type)
  | None ->
      let path_str = String.concat "::" (fst path @ [snd path]) in
      let error_msg = Printf.sprintf "Unresolved name: %s" path_str in
      let ctx' = add_error ctx location error_msg in
      (ctx', TypeError error_msg)

(* =================================================================================================
   FUNCTION APPLICATION TYPE INFERENCE
   ================================================================================================= *)

(* Infer type of function application *)
let infer_function_application ctx func_expr type_args const_args args location =
  (* Step 1: Infer function type *)
  let ctx, func_result = infer_expr_type ctx func_expr in
  
  match func_result with
  | Success func_type ->
      (* Step 2: Infer argument types *)
      let ctx, arg_results = 
        List.fold_left_map (fun ctx arg ->
          let ctx', result = infer_expr_type ctx arg in
          (ctx', result)
        ) ctx args in
      
      (* Step 3: Check if all arguments typed successfully *)
      let arg_types = List.map (function
        | Success typ -> typ
        | _ -> make_typ_info (TVar ("$error")) ~inferred:true ()) arg_results in
      
      (* Step 4: Apply function type (simplified) *)
      (* TODO: Implement proper function type application with generics *)
      let return_type = make_typ_info (fresh_type_var ctx) ~inferred:true () in
      (ctx, Success return_type)
      
  | error -> (ctx, error)

(* =================================================================================================
   MAIN TYPE INFERENCE FUNCTION
   ================================================================================================= *)

(* Infer type of expression node *)
and infer_expr_type ctx expr =
  let location = expr.meta.location in
  
  (* If expression already has explicit type, use it *)
  match expr.typ with
  | Some typ_info when not typ_info.inferred ->
      (ctx, Success typ_info)
  | _ ->
      (* Infer from expression structure *)
      match expr.node with
      
      (* Literals - always succeed *)
      | ELit literal ->
          let typ = infer_literal_type literal in
          (ctx, Success typ)
      
      (* Variables *)
      | EVar name ->
          lookup_variable ctx name location
      
      (* Qualified names *)
      | EQualified path ->
          lookup_qualified_name ctx path location
      
      (* Function application *)
      | EApp { func; type_args; const_args; args } ->
          infer_function_application ctx func type_args const_args args location
      
      (* Let bindings *)
      | ELet { var; var_type; init; body } ->
          (* Infer initializer type *)
          let ctx, init_result = infer_expr_type ctx init in
          
          (match init_result with
          | Success init_type ->
              (* Check against explicit type annotation if present *)
              let ctx, final_var_type = 
                match var_type with
                | Some explicit_type ->
                    let ctx' = unify_types ctx init_type explicit_type 
                      ("let binding type annotation") in
                    (ctx', explicit_type)
                | None -> (ctx, init_type) in
              
              (* Add variable to environment and infer body *)
              let env' = add_var ctx.env var final_var_type false LetBinding in
              let ctx' = { ctx with env = env' } in
              infer_expr_type ctx' body
              
          | error -> (ctx, error))
      
      (* Type ascription *)
      | EAscribe (expr, typ) ->
          let ctx, expr_result = infer_expr_type ctx expr in
          (match expr_result with
          | Success expr_type ->
              let ctx' = unify_types ctx expr_type typ "type ascription" in
              (ctx', Success typ)
          | error -> (ctx, error))
      
      (* Control flow - simplified implementations *)
      | EIf (cond, then_expr, else_expr) ->
          (* TODO: Implement proper if expression inference *)
          let return_type = make_typ_info (fresh_type_var ctx) ~inferred:true () in
          (ctx, Success return_type)
      
      | ESequence exprs ->
          (* Type of sequence is type of last expression *)
          (match List.rev exprs with
          | [] -> (ctx, Success (make_typ_info TUnit ~inferred:true ()))
          | last :: _ -> infer_expr_type ctx last)
      
      (* Placeholders for other expression types *)
      | EMatch (_, _) -> 
          (ctx, NeedsAnnotation "Match expressions require type annotations")
      | EWhile (_, _) ->
          (ctx, Success (make_typ_info TUnit ~inferred:true ()))
      | ERecord _ ->
          (ctx, NeedsAnnotation "Record expressions require type annotations")
      | ETuple exprs ->
          (ctx, NeedsAnnotation "Tuple expressions require type annotations")
      | EArray _ ->
          (ctx, NeedsAnnotation "Array expressions require type annotations")
      | EAddrOf _ ->
          (ctx, NeedsAnnotation "Address-of expressions require type annotations")
      | EDeref _ ->
          (ctx, NeedsAnnotation "Deref expressions require type annotations")
      | EIndex (_, _) ->
          (ctx, NeedsAnnotation "Index expressions require type annotations")
      | EField (_, _) ->
          (ctx, NeedsAnnotation "Field access requires type annotations")
      | EMethodCall _ ->
          (ctx, NeedsAnnotation "Method calls require type annotations")

(* =================================================================================================
   PUBLIC API
   ================================================================================================= *)

(* Create inference context *)
let create_inference_ctx env = {
  env;
  constraints = [];
  type_var_counter = ref 0;
  errors = [];
}

(* Main type inference entry point *)
let infer_expression_type env expr =
  let ctx = create_inference_ctx env in
  let final_ctx, result = infer_expr_type ctx expr in
  
  (* Check for errors *)
  match final_ctx.errors with
  | [] -> result
  | errors ->
      let error_messages = List.map snd (List.rev errors) in
      TypeError (String.concat "; " error_messages)

(* Check if expression is well-typed *)
let check_expression_type env expr expected_type =
  let ctx = create_inference_ctx { env with expected_type = Some expected_type } in
  let final_ctx, result = infer_expr_type ctx expr in
  
  match result with
  | Success inferred_type ->
      let final_ctx' = unify_types final_ctx inferred_type expected_type 
        "expression type check" in
      (match final_ctx'.errors with
      | [] -> Success expected_type
      | errors -> TypeError (String.concat "; " (List.map snd errors)))
  | error -> error

(* =================================================================================================
   UTILITY FUNCTIONS FOR DEVELOPMENT
   ================================================================================================= *)

(* Pretty print inference result *)
let string_of_inference_result = function
  | Success typ -> "Success: " ^ string_of_type typ.typ
  | NeedsAnnotation reason -> "Needs annotation: " ^ reason
  | TypeError error -> "Type error: " ^ error

(* Debug function to show inference process *)
let debug_infer_expression env expr =
  Printf.printf "Inferring type for expression...\n";
  let result = infer_expression_type env expr in
  Printf.printf "Result: %s\n" (string_of_inference_result result);
  result

(* =================================================================================================
   IMPLEMENTATION NOTES
   ================================================================================================= *)

(*
   CURRENT LIMITATIONS:
   - Simplified unification algorithm (no occurs check, substitution)
   - Limited function type handling (no proper generics)
   - No const generic evaluation
   - No method resolution
   - Basic error reporting
   
   NEXT STEPS:
   1. Implement proper unification with substitution
   2. Add support for generic function instantiation
   3. Integrate with Krml type system
   4. Add const generic evaluation
   5. Implement method resolution
   6. Improve error messages with suggestions
   
   TESTING STRATEGY:
   - Unit tests for each inference case
   - Property-based tests for unification
   - Integration tests with real expressions
   - Error message quality tests
*)