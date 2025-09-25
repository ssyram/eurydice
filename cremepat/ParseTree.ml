(* =================================================================================================
   CREMEPAT PARSE TREE: Core Data Structures for Pattern Matching Syntax
   =================================================================================================
   
   This file defines the Abstract Syntax Tree (AST) for cremepat's pattern matching language.
   Cremepat allows writing patterns that match against OCaml AST nodes in a readable syntax
   instead of deeply nested OCaml pattern matching expressions.
   
   CORE CONCEPT: The 'with_vars' wrapper
   ------------------------------------
   The key innovation of cremepat is the 'with_vars wrapper that allows expressions, patterns,
   and types to contain unification variables. This enables flexible matching where parts of
   the AST can be captured into variables or left as wildcards.
   
   - Fixed('a) : A concrete/literal AST node
   - PatternVar(string) : A unification variable that captures a single AST node (syntax: ?name)  
   - ListPatternVar(string) : A list unification variable that captures a sequence (syntax: ?name..)
   
   LEARNING PATH: Read this file in order:
   1. 'with_vars type and its purpose
   2. Basic expression constructs (pre_expr)
   3. Path system for qualified names
   4. Pattern and type representations
 *)

(* The fundamental wrapper type that enables unification variables in patterns.
   This is the foundation of cremepat's flexibility - any AST node can either be:
   - A concrete value (Fixed)
   - A pattern variable that captures one element (?var)
   - A list pattern variable that captures multiple elements (?var..)
 *)
and 'a with_vars = 
  | PatternVar of string      (* ?name - captures single AST node *)
  | ListPatternVar of string  (* ?name.. - captures list of AST nodes *)  
  | Fixed of 'a              (* concrete literal value *)

(* Expression AST: Represents the structure of expressions in the target language.
   This mirrors the structure of the actual Krml/OCaml AST that cremepat patterns match against.
   Each constructor corresponds to a specific kind of expression in the target language.
   
   DESIGN PRINCIPLE: These constructors match the target AST structure exactly, allowing
   direct translation from cremepat patterns to OCaml pattern matches.
 *)
type pre_expr =
  (* Control flow and binding constructs (loosely binding) *)
  | Let of string * expr * expr      (* let x = e1 in e2 *)
  | Sequence of expr list           (* e1; e2; e3 (sequence of expressions) *)
  
  (* Function application with full Rust/generics support *)
  | App of { 
      head : expr;              (* function being called *)
      cgs : expr list;          (* const generic arguments [#...] *)
      methods : expr list;      (* method arguments (#...) *)  
      ts : typ list;           (* type arguments <...> *)
      args : expr list         (* regular function arguments (...) *)
    }
    
  (* Memory and indexing operations *)
  | Addr of expr               (* &expr (address-of) *)
  | Index of expr * expr       (* expr[index] *)
  
  (* Control structures (atomic - terminated by braces) *)
  | While of expr * expr       (* while condition { body } *)
  | Match of expr * branch list (* match expr { pat => expr, ... } *)
  
  (* Data structures and literals *)
  | Record of (string * expr) list  (* { field: expr, ... } *)
  | Int of int                     (* integer literals *)
  | Bool of bool                   (* boolean literals *)
  
  (* Variable and path references *)
  | Qualified of path              (* module::path::name *)
  | BoundVar of string             (* local variable reference *)
  | Break                         (* break statement *)

(* Expression with unification variables - this is the main expression type *)
and expr = pre_expr with_vars

(* Path system: Represents qualified names like std::vec::Vec or local::function
   This is crucial for matching against complex module paths in generated code.
   
   Example: std::collections::HashMap becomes:
   [Name "std"; Name "collections"; Name "HashMap"]
   
   With pattern variables: std::?module::HashMap becomes:
   [Name "std"; Var "module"; Name "HashMap"]
 *)
and path = path_item list
and path_item = 
  | Name of string    (* concrete path component *)
  | Wild             (* _ wildcard *)
  | Var of string    (* ?name pattern variable *)

(* Pattern matching constructs *)  
and branch = pat * expr                    (* pattern => expression *)
and pre_pat = Cons of string * pat list   (* Constructor(pat1, pat2, ...) *)
and pat = pre_pat with_vars              (* pattern with unification variables *)

(* Type system representation *)
and pre_typ = 
  | TQualified of path           (* qualified type name *)
  | TApp of typ * typ list      (* type application: T<A, B, ...> *)
and typ = pre_typ with_vars     (* type with unification variables *)

(* ===============================================================================
   UTILITY FUNCTIONS
   =============================================================================== *)

(* Symbol generation for temporary variables.
   Used when cremepat needs to create fresh variable names during compilation.
   This ensures no name collisions occur in the generated patterns.
 *)
let gensym =
  let r = ref 0 in
  fun () ->
    incr r;
    "$x" ^ string_of_int !r

(* =================================================================================================
   IMPLEMENTING EXPRESSIONS EXTENSION: Design Analysis & Recommendations  
   =================================================================================================

   Based on the pattern extension architecture, here's a comprehensive analysis of how to 
   implement an expressions extension for cremepat:

   KEY CHALLENGE: TYPE INFERENCE AND VALIDATION
   -------------------------------------------
   Unlike patterns, expressions must be well-typed. The main challenge is inferring and
   validating types during cremepat compilation. Consider these approaches:

   1. TYPE-DIRECTED COMPILATION APPROACH:
   ------------------------------------
   - Extend ParseTree with explicit type annotations
   - Add type environment to track variable types during compilation  
   - Implement type inference rules for each expression construct
   - Validate type correctness before generating OCaml expressions

   Example extended ParseTree for expressions:
   
   type expr_with_type = { 
     node : pre_expr; 
     typ : typ option;  (* explicit type annotation *)
     inferred_typ : typ option (* inferred type *)
   }

   2. ENVIRONMENT MANAGEMENT FOR EXPRESSIONS:
   ----------------------------------------
   The current 'env = string list' is insufficient for expressions. Need:
   
   type expr_env = {
     vars : (string * typ) list;        (* variable name -> type *)
     type_vars : string list;           (* type variables in scope *)
     const_generics : (string * typ) list; (* const generic parameters *)
   }

   3. TYPE INFERENCE ALGORITHM:
   ---------------------------
   For each expression construct, implement typing rules:
   
   - Let expressions: 
     * Infer type of RHS expression
     * Extend environment with binding  
     * Type-check body in extended environment
     
   - Function calls:
     * Resolve function type from qualified path
     * Apply type arguments and const generics
     * Unify parameter types with argument types
     
   - Variables:
     * Look up type in environment
     * Handle both bound variables and qualified references

   4. RECOMMENDED IMPLEMENTATION STEPS:
   ----------------------------------
   
   Step 1: Extend ParseTree.ml
   - Add type annotation syntax to grammar
   - Support explicit type ascription (expr : typ)
   - Add type inference result fields
   
   Step 2: Extend Parse.mly 
   - Add grammar rules for type annotations
   - Support expression context in addition to pattern context
   - Add precedence rules for type ascription
   
   Step 3: Create ExprCompiler module
   - Implement type inference engine
   - Add type environment management
   - Generate well-typed OCaml expressions
   
   Step 4: Integration with existing AST
   - Ensure generated expressions match target AST structure
   - Handle const generics and method calls properly
   - Support complex path resolution

   5. SPECIFIC TECHNICAL CHALLENGES:
   --------------------------------
   
   a) CONST GENERICS:
      Unlike patterns, expressions with const generics must resolve to
      specific concrete values. Need compile-time evaluation:
      
      let eval_const_generic env (cg : expr) : typ =
        (* evaluate const generic expression to concrete type *)
   
   b) METHOD RESOLUTION:  
      Method calls require understanding of trait implementations.
      May need integration with Rust's type system information.
      
   c) POLYMORPHIC FUNCTIONS:
      Function calls may require instantiating polymorphic types:
      
      Vec::new<T>() (* T must be inferred from context *)
      
   d) LIFETIME PARAMETERS:
      Expressions may need to track lifetime information for 
      reference types and borrowing.

   6. SYNTAX DESIGN CONSIDERATIONS:
   ------------------------------
   
   For expression extension, consider syntax like:
   
   [%cremeexpr {| 
     let x : u32 = ?init_val in
     std::vec::Vec::<u32>::with_capacity(?cap).push(x)
   |}]
   
   Key syntax elements:
   - Explicit type annotations (: u32)  
   - Pattern variables (?var) for capturing subexpressions
   - Support for complex generic instantiation
   - Type-directed code generation

   7. ERROR HANDLING & DIAGNOSTICS:
   -------------------------------
   
   Expression compilation needs comprehensive error reporting:
   - Type mismatch errors with location information
   - Unresolved variable/function errors  
   - Generic instantiation failures
   - Const generic evaluation errors
   
   This is much more complex than pattern compilation which can
   ignore most type information.

   RECOMMENDATION: Start with a minimal subset supporting basic expressions
   (variables, function calls, literals) and gradually add complexity.
 *)
