(* =================================================================================================
   CREMEPAT EXPRESSION PARSE TREE: AST for Expression Generation
   =================================================================================================
   
   This file defines the AST for cremepat's expression generation language.
   Unlike the pattern matching ParseTree.ml, this focuses on generating well-typed expressions
   that match the Krml.Ast structure.
   
   KEY DIFFERENCES FROM PATTERNS:
   - Type information is crucial and tracked throughout
   - Expressions must be well-formed and type-checked  
   - Support for both concrete expressions and pattern variables
   - Integration with type inference system
 *)

(* =================================================================================================
   CORE TYPE SYSTEM
   ================================================================================================= *)

(* Type representation - mirrors Krml type system *)
type base_typ =
  | TInt of Krml.Ast.width       (* i32, u64, etc. *)
  | TBool                        (* boolean type *)
  | TStr                         (* string type *)
  | TUnit                        (* unit type *)
  | TVar of string               (* type variable: 'a, 'b *)
  | TQualified of path           (* qualified type: std::vec::Vec *)
  | TApp of base_typ * base_typ list  (* generic application: Vec<i32> *)

and path = string list * string  (* (["std"; "vec"], "Vec") *)

(* Type with optional inference information *)
type typ_info = {
  typ : base_typ;
  inferred : bool;          (* true if inferred, false if explicit *)
  source_loc : location option;
}

and location = {
  line : int;
  column : int;
  file : string;
}

(* =================================================================================================
   EXPRESSION AST WITH TYPE INFORMATION
   ================================================================================================= *)

(* Expression nodes with type tracking *)
type expr_node =
  (* Literals with known types *)
  | ELit of literal
  
  (* Variables and qualified names *)
  | EVar of string                    (* local variable *)
  | EQualified of path               (* module::function *)
  
  (* Function application with full Rust syntax *)
  | EApp of {
      func : expr;                   (* function being called *)
      type_args : typ_info list;     (* <T, U> type parameters *)
      const_args : expr list;        (* [#N, M] const generic parameters *)
      args : expr list;              (* (a, b, c) regular arguments *)
    }
    
  (* Let bindings with type inference *)
  | ELet of {
      var : string;
      var_type : typ_info option;    (* optional type annotation *)
      init : expr;
      body : expr;
    }
    
  (* Type ascription for explicit typing *)
  | EAscribe of expr * typ_info      (* (expr : typ) *)
  
  (* Control flow *)
  | EIf of expr * expr * expr option  (* if cond then expr else expr *)
  | EMatch of expr * branch list      (* match expr with pat => expr, ... *)
  | EWhile of expr * expr             (* while cond { body } *)
  | ESequence of expr list            (* expr1; expr2; expr3 *)
  
  (* Data structures *)
  | ERecord of (string * expr) list   (* { field: expr, ... } *)
  | ETuple of expr list               (* (a, b, c) *)
  | EArray of expr list               (* [a, b, c] *)
  
  (* Memory operations *)
  | EAddrOf of expr                   (* &expr *)
  | EDeref of expr                    (* *expr *)
  | EIndex of expr * expr             (* expr[index] *)
  | EField of expr * string           (* expr.field *)
  
  (* Method calls (syntactic sugar for function calls) *)
  | EMethodCall of {
      receiver : expr;
      method_name : string;
      type_args : typ_info list;
      args : expr list;
    }

(* Expression with type and meta information *)
and expr = {
  node : expr_node;
  typ : typ_info option;           (* type information (inferred or explicit) *)
  meta : expr_meta;
}

(* Metadata for expressions *)
and expr_meta = {
  location : location option;
  annotations : string list;       (* user annotations like @inline *)
}

(* Literals with type information *)
and literal =
  | LInt of int * Krml.Ast.width option    (* 42, 42u32 *)
  | LBool of bool                          (* true, false *)
  | LStr of string                         (* "hello" *)
  | LUnit                                  (* () *)

(* Pattern matching branches *)
and branch = pattern * expr

(* Patterns for match expressions - reuse from existing system where possible *)
and pattern = 
  | PCons of string * pattern list     (* Constructor(pat, ...) *)
  | PVar of string                     (* variable binding *)
  | PWild                              (* _ wildcard *)
  | PLit of literal                    (* literal pattern *)

(* =================================================================================================
   UNIFICATION VARIABLES FOR MIXED CONCRETE/PATTERN EXPRESSIONS
   ================================================================================================= *)

(* Enhanced with_vars that supports type information *)
type 'a with_vars_typed = 
  | Fixed of 'a                        (* concrete value *)
  | PatternVar of string * typ_info option  (* ?x with optional type hint *)
  | ListPatternVar of string * typ_info option  (* ?xs.. with optional element type *)

(* Top-level expression type that can contain pattern variables *)
type expr_with_vars = expr_node with_vars_typed

(* =================================================================================================
   TYPE ENVIRONMENT FOR INFERENCE AND CHECKING
   ================================================================================================= *)

type type_binding = {
  name : string;
  typ : typ_info;
  mutable_flag : bool;
  source : binding_source;
}

and binding_source =
  | LetBinding                        (* from let x = ... *)
  | Parameter                         (* function parameter *)
  | PatternMatch                      (* from pattern matching *)

type type_env = {
  (* Variable bindings *)
  vars : type_binding list;
  
  (* Type variables in scope *)
  type_vars : string list;
  
  (* Const generic variables *)
  const_generics : (string * typ_info) list;
  
  (* Function signatures (simplified) *)
  functions : (path * function_signature) list;
  
  (* Expected type context for inference *)
  expected_type : typ_info option;
  
  (* Current module path for resolution *)
  module_path : string list;
}

and function_signature = {
  type_params : string list;          (* <T, U> *)
  const_params : string list;         (* [#N, M] *)
  param_types : typ_info list;        (* parameter types *)
  return_type : typ_info;             (* return type *)
}

(* =================================================================================================
   TYPE INFERENCE RESULT TYPES
   ================================================================================================= *)

type inference_result = 
  | Success of typ_info
  | NeedsAnnotation of string        (* reason why annotation is needed *)
  | TypeError of string              (* type error message *)

type unification_constraint = {
  left : typ_info;
  right : typ_info;
  reason : string;                   (* why these types must be equal *)
}

(* =================================================================================================
   UTILITY FUNCTIONS
   ================================================================================================= *)

(* Create empty type environment *)
let empty_env = {
  vars = [];
  type_vars = [];
  const_generics = [];
  functions = [];
  expected_type = None;
  module_path = [];
}

(* Add variable binding to environment *)
let add_var env name typ mutable_flag source =
  let binding = { name; typ; mutable_flag; source } in
  { env with vars = binding :: env.vars }

(* Look up variable type *)
let lookup_var env name =
  List.find_opt (fun binding -> binding.name = name) env.vars

(* Create type info *)
let make_typ_info typ ~inferred ?location () = {
  typ;
  inferred;
  source_loc = location;
}

(* Create expression with metadata *)
let make_expr node ?typ ?location ?(annotations = []) () = {
  node;
  typ;
  meta = { location; annotations };
}

(* =================================================================================================
   DESIGN NOTES AND IMPLEMENTATION GUIDANCE
   ================================================================================================= *)

(*
   IMPLEMENTATION PHASES:
   
   Phase 1: Basic expressions (literals, variables, simple function calls)
   - Implement lexing/parsing for basic syntax
   - Simple type inference for literals and variables
   - PPX integration for [%cremeexpr ...]
   
   Phase 2: Let bindings and type propagation
   - Environment management
   - Type constraint generation and solving
   - Pattern variables in expressions
   
   Phase 3: Generics and advanced types
   - Type parameter instantiation
   - Const generic evaluation
   - Method call resolution
   
   Phase 4: Control flow and integration
   - If, match, while expressions
   - Integration with existing pattern system
   - Error handling and diagnostics
   
   TYPE INFERENCE STRATEGY:
   - Start with simple local inference
   - Use expected types to guide inference direction
   - Fallback to requiring explicit annotations
   - Generate clear error messages when inference fails
   
   ERROR HANDLING PRIORITIES:
   1. Clear error messages with location information
   2. Suggestions for fixing type errors
   3. Graceful degradation when inference fails
   4. Integration with OCaml's error reporting
*)