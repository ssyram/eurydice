(* =================================================================================================
   CREMEPAT COMPILER: Transforming Pattern Language to OCaml Patterns
   =================================================================================================
   
   This module is the heart of the cremepat PPX extension. It compiles cremepat's custom
   pattern matching syntax into standard OCaml pattern matches against AST nodes.
   
   ARCHITECTURE OVERVIEW:
   ---------------------
   1. PARSING: Text input → ParseTree AST (via Parse.mly + Lex.ml)  
   2. COMPILATION: ParseTree → OCaml Pattern AST (this module)
   3. INTEGRATION: OCaml patterns embedded in source code (via Ppxlib)
   
   KEY INSIGHT: Pattern Transformation
   ----------------------------------
   Cremepat patterns like:  {| let ?x = ?init; ?body |}
   Become OCaml patterns: { node = ELet (_, { node = EBound 0; _ }, body_pattern); _ }
   
   This transformation enables readable pattern matching against complex AST structures
   without writing deeply nested OCaml pattern matches manually.
   
   LEARNING PATH:
   1. Error handling and parsing infrastructure (this section)
   2. Environment management for variable scoping
   3. Pattern AST construction helpers  
   4. Core compilation algorithms
   5. PPX integration and extension registration
 *)

(* =================================================================================================
   ERROR HANDLING AND DIAGNOSTICS
   ================================================================================================= *)

(* Terminal color codes for pretty error reporting *)
module Terminal = struct
  let mkcolor x = Printf.sprintf "\x1b[38;5;%dm" x
  let green = mkcolor 119
  let red = mkcolor 203  
  let blue = mkcolor 81
  let yellow = mkcolor 227
  let orange = mkcolor 202
  let underline = "\x1b[4m"
  let reset = "\x1b[0m"
end

(* Printf-style error reporting with automatic string formatting *)
let fail fmt =
  let b = Buffer.create 256 in
  Printf.kbprintf (fun b -> failwith (Buffer.contents b)) b fmt

(* =================================================================================================
   PARSING INFRASTRUCTURE: From Text to ParseTree
   ================================================================================================= *)

(* Main parsing function: converts cremepat syntax strings to ParseTree AST nodes.
   
   PROCESS:
   1. Create Sedlex lexing buffer from input string
   2. Use Menhir parser with custom lexer to build AST  
   3. Handle lexing and parsing errors with detailed diagnostics
   4. Return ParseTree.expr on success
   
   ERROR HANDLING:
   - Lexing errors: malformed unicode or invalid characters
   - Parsing errors: syntax errors with precise location info and visual indicators
 *)
let parse arg =
  (* Convert Menhir's incremental API to simplified interface *)
  let the_parser = MenhirLib.Convert.Simplified.traditional2revised Parse.fragment in
  let lexbuf = Sedlexing.Utf8.from_string arg in
  
  try 
    (* Parse the input using our custom lexer *)
    the_parser (fun _ -> Lex.token lexbuf) 
  with
  
  (* Lexical analysis errors *)  
  | (Sedlexing.MalFormed | Sedlexing.InvalidCodepoint _) as e ->
      Printf.eprintf "Lexing error in: %s\n" arg;
      raise e
      
  (* Parse errors with detailed location reporting *)
  | Parse.Error as e ->
      (* Extract error location from lexer state *)
      let start, end_ = Sedlexing.loc lexbuf in
      let start = start - !Lex.cur_line in
      let end_ = end_ - !Lex.cur_line in
      
      (* Build visual error indicator *)
      let buf = Buffer.create 256 in
      List.iteri
        (fun i line ->
          Buffer.add_string buf line;
          Buffer.add_char buf '\n';
          (* Add error indicators on the line where parsing failed *)
          if i + 1 = !Lex.lines then begin
            Buffer.add_string buf Terminal.red;
            (* Add spaces to align with error position *)
            for _j = 0 to start do
              Buffer.add_char buf ' '
            done;
            (* Add carets to indicate error span *)
            for _j = start to end_ - 1 do
              Buffer.add_char buf '^'
            done;
            Buffer.add_string buf Terminal.reset;
            Buffer.add_char buf '\n'
          end)
        (String.split_on_char '\n' arg);
        
      (* Report detailed error with location and visual feedback *)
      Printf.eprintf "Parse error, line %d, characters %d-%d: %s\n" 
        !Lex.lines start end_ (Buffer.contents buf);
      raise e

open Ppxlib

(* =================================================================================================
   ENVIRONMENT MANAGEMENT: Variable Scoping and Binding
   ================================================================================================= *)

(* Compilation environment tracks bound variables during pattern compilation.
   
   PURPOSE: When cremepat encounters BoundVar references in patterns, it needs to
   resolve them to De Bruijn indices (0-based positions from innermost binding).
   
   EXAMPLE: In pattern "let x = ?init in let y = x + ?val in y"
   - Environment starts empty: []
   - After "let x": ["x"] 
   - After "let y": ["y"; "x"]
   - Reference to "x" resolves to index 1 (second from top)
   - Reference to "y" resolves to index 0 (topmost binding)
   
   This matches the target AST's variable representation using De Bruijn indices.
 *)
type env = string list

let empty = []
let push env x = x :: env  (* Add new binding to front (most recent) *)

(* Resolve variable name to De Bruijn index *)
let find env x =
  let exception Found of int in
  try
    List.iteri
      (fun i x' ->
        if x = x' then
          raise (Found i))  (* Return position from start of list *)
      env;
    raise Not_found
  with Found i -> i

(* =================================================================================================
   PATTERN AST CONSTRUCTION HELPERS
   ================================================================================================= *)

(* These functions build OCaml pattern AST nodes using Ppxlib.
   They abstract away the complexity of constructing AST nodes manually.
   
   DESIGN PRINCIPLE: Each helper corresponds to a specific OCaml pattern construct:
   - ppat_construct: variant constructors (Some, None, ELet, etc.)
   - ppat_tuple: tuple patterns (a, b, c)  
   - ppat_record: record patterns { field = pattern; ... }
   - ppat_constant: literals (42, "hello", true)
   - ppat_var: variable bindings (?x captures to variable x)
 *)

(* Create longident for constructor names *)
let lident ~loc x = { txt = Lident x; loc }

(* Build list patterns using :: constructor recursively *)
let rec ppat_list ~loc pats =
  let open Ast_builder.Default in
  List.fold_right
    (fun pat acc -> ppat_construct ~loc (lident ~loc "::") (Some (ppat_tuple ~loc [ pat; acc ])))
    pats
    (ppat_construct ~loc (lident ~loc "[]") None)

(* =================================================================================================
   VARIANT CONSTRUCTOR PATTERNS
   ================================================================================================= *)

(* These functions generate patterns for algebraic data type constructors.
   The target AST uses constructors like ELet, EApp, EBound to represent different expression types.
   
   Pattern generation follows this scheme:
   - Zero arguments: Some → { node = Some; _ }
   - One argument: Some(x) → { node = Some(x); _ } 
   - Multiple arguments: Cons(a,b,c) → { node = Cons((a,b,c)); _ }
 *)

(* Multi-argument constructor: Cons(arg1, arg2, ...) *)  
let ppat_cons_many' ~loc cons args =
  let open Ast_builder.Default in
  ppat_construct ~loc (lident ~loc cons) (Some (ppat_tuple ~loc args))

(* Single-argument constructor: Some(arg) *)
let ppat_cons_one' ~loc cons arg =
  let open Ast_builder.Default in
  ppat_construct ~loc (lident ~loc cons) (Some arg)

(* Zero-argument constructor: None *)
let ppat_cons_zero' ~loc cons =
  let open Ast_builder.Default in
  ppat_construct ~loc (lident ~loc cons) None

(* =================================================================================================
   LITERAL PATTERN CONSTRUCTORS
   ================================================================================================= *)

(* Generate patterns for literal values that appear in cremepat patterns *)

(* String literal pattern: "hello" *)
let ppat_string ~loc s =
  let open Ast_builder.Default in
  ppat_constant ~loc (Pconst_string (s, loc, None))

(* Integer literal pattern: 42 *)  
let ppat_int ~loc s =
  let open Ast_builder.Default in
  ppat_constant ~loc (Pconst_integer (string_of_int s, None))

(* Boolean literal pattern: true/false *)
let ppat_bool ~loc s = ppat_cons_zero' ~loc (string_of_bool s)

(* =================================================================================================
   TARGET AST WRAPPER: The { node = ...; _ } Pattern
   ================================================================================================= *)

(* The target Krml AST wraps expression nodes in records like { node = ELet(...); typ = ...; meta = ... }.
   Cremepat patterns must match this structure, so we generate patterns that extract the 'node' field
   while ignoring other fields using record pattern matching with Open.
   
   EXAMPLE TRANSFORMATION:
   cremepat: let ?x = ?init; ?body
   OCaml pattern: { node = ELet (_, { node = ?init_pattern; _ }, { node = ?body_pattern; _ }); _ }
 *)

(* Wrap a node pattern in the standard { node = ...; _ } record pattern *)
let ppat_node ~loc pat =
  let open Ast_builder.Default in
  ppat_record ~loc [ lident ~loc "node", pat ] Open  (* Open allows ignoring other record fields *)

(* Convenient wrappers that automatically apply the node wrapper *)
let ppat_cons_many ~loc cons args = ppat_node ~loc (ppat_cons_many' ~loc cons args)
let ppat_cons_one ~loc cons arg = ppat_node ~loc (ppat_cons_one' ~loc cons arg)  
let ppat_cons_zero ~loc cons = ppat_node ~loc (ppat_cons_zero' ~loc cons)

(* =================================================================================================
   CORE COMPILATION ALGORITHM: From ParseTree to OCaml Patterns
   ================================================================================================= *)

(* This is the heart of cremepat: the recursive compilation of ParseTree AST nodes
   into OCaml pattern AST nodes.
   
   ARCHITECTURE:
   - compile_with_var: handles unification variables (?x, ?x..)
   - compile_list_pattern: handles lists that may contain list variables (?x..)  
   - compile_pre_*: handles concrete AST node types
   - compile_*: entry points that delegate to appropriate handlers
   
   KEY INSIGHT: Mutual Recursion
   The compilation functions are mutually recursive because:
   - Expressions can contain patterns (in match arms)
   - Patterns can contain expressions (in guard conditions)  
   - Types can contain expressions (in const generic arguments)
   - All can contain unification variables
 *)

let compile_parse_tree (env : env) loc (pt : ParseTree.expr) =
  let open Ast_builder.Default in
  
  (* =================================================================================================
     UNIFICATION VARIABLE HANDLING
     ================================================================================================= *)
  
  (* Compile AST nodes that may contain unification variables.
     
     ALGORITHM:
     1. If Fixed(node): recursively compile the concrete node
     2. If PatternVar(name): generate OCaml variable pattern binding
     3. If ListPatternVar(name): error - list variables not allowed in single positions
     
     This function enforces the typing constraint that list variables (?x..) 
     can only appear in list contexts, not single element contexts.
   *)
  let rec compile_with_var : 'a. env -> 'a ParseTree.with_vars -> (env -> 'a -> _) -> _ =
   fun env pt compile_pre ->
    match pt with
    | Fixed e -> compile_pre env e                    (* Delegate to concrete node compiler *)
    | PatternVar txt -> ppat_var ~loc { txt; loc }    (* Generate variable binding pattern *)
    | ListPatternVar s -> 
        fail "[cremepat]: list pattern ?%s.. appears in unexpected position" s
  
  (* Compile lists that may contain list unification variables.
     
     ALGORITHM:
     1. Empty list [] → OCaml empty list pattern []
     2. Single list variable [?x..] → OCaml variable pattern x  
     3. List variable in non-tail position → error (violates typing constraints)
     4. Regular pattern variable ?x → cons pattern x :: rest
     5. Concrete element → cons pattern element :: rest
     
     IMPORTANT: List variables (?x..) can only appear in tail position to maintain
     the invariant that they capture "the rest of the list".
   *)
  and compile_list_pattern : 'a. env -> 'a ParseTree.with_vars list -> (env -> 'a -> _) -> _ =
   fun env pts compile_pre ->
    match pts with
    | [] -> ppat_construct ~loc (lident ~loc "[]") None
    | ListPatternVar txt :: [] -> ppat_var ~loc { txt; loc }    (* Tail position: OK *)
    | ListPatternVar s :: _ ->
        fail "[cremepat]: list pattern ?%s.. should only appear in tail position" s
    | PatternVar txt :: pts ->
        (* Regular variable in cons position *)
        ppat_construct ~loc (lident ~loc "::")
          (Some (ppat_tuple ~loc 
            [ ppat_var ~loc { txt; loc }; compile_list_pattern env pts compile_pre ]))
    | Fixed pt :: pts ->
        (* Concrete element in cons position *)  
        ppat_construct ~loc (lident ~loc "::")
          (Some (ppat_tuple ~loc [ compile_pre env pt; compile_list_pattern env pts compile_pre ]))

  (* =================================================================================================
     EXPRESSION COMPILATION: The Main Logic
     ================================================================================================= *)
     
  (* Entry points for different AST node types *)
  and compile env pt = compile_with_var env pt compile_pre_expr
  and compile_expr_list_pattern env pt = compile_list_pattern env pt compile_pre_expr
  
  (* Compile concrete expression nodes to OCaml patterns.
     
     This function maps each ParseTree expression constructor to the corresponding
     pattern that matches against the target Krml AST structure.
     
     PATTERN MAPPING EXAMPLES:
     - Let(b, e1, e2) → ELet(_, e1_pattern, e2_pattern)
     - App{head;args;...} → EApp(ETApp(head_pattern, ...), args_pattern)
     - BoundVar(s) → EBound(index) where index = position of s in environment
   *)
  and compile_pre_expr env pt =
    match pt with
    
    (* Let binding: let x = e1 in e2 *)
    | ParseTree.Let (b, e1, e2) ->
        let p1 = compile env e1 in
        let env = push env b in              (* Extend environment with new binding *)
        let p2 = compile env e2 in
        (* Generate pattern: ELet (_, e1_pattern, e2_pattern) *)
        (* First wildcard matches the type annotation which we ignore in patterns *)
        ppat_cons_many ~loc "ELet" [ ppat_any ~loc; p1; p2 ]
        
    (* Expression sequence: e1; e2; e3 *)  
    | Sequence ps -> 
        ppat_cons_one ~loc "ESequence" (compile_expr_list_pattern env ps)
        
    (* Complex function application with full Rust syntax:
       f[#cg1,cg2](#m1,m2)<T1,T2>(arg1,arg2) 
       
       Maps to: EApp (ETApp (head, cgs, methods, ts), args)
     *)
    | App { head; cgs; methods; ts; args } ->
        ppat_cons_many ~loc "EApp"
          [
            ppat_cons_many ~loc "ETApp"
              [
                compile env head;                           (* Function being called *)
                compile_expr_list_pattern env cgs;          (* Const generic args *)
                compile_expr_list_pattern env methods;      (* Method args *)  
                compile_typ_list_pattern env ts;           (* Type args *)
              ];
            ppat_list ~loc (List.map (compile env) args);  (* Function arguments *)
          ]
          
    (* Address-of operator: &expr *)
    | Addr e -> 
        ppat_cons_one ~loc "EAddrOf" (compile env e)
        
    (* Array/buffer indexing: expr[index] *) 
    | Index (e1, e2) ->
        let p1 = compile env e1 in
        let p2 = compile env e2 in
        ppat_cons_many ~loc "EBufRead" [ p1; p2 ]
        
    (* While loop: while condition { body } *)
    | While (e1, e2) ->
        let p1 = compile env e1 in
        let p2 = compile env e2 in
        ppat_cons_many ~loc "EWhile" [ p1; p2 ]
        
    (* Pattern matching: match expr { pat => expr, ... } *)
    | Match (e, bs) ->
        let e = compile env e in
        ppat_cons_many ~loc "EMatch"
          [
            ppat_any ~loc;                    (* Match flavor wildcard - no syntax to specify *)
            e;                               (* Expression being matched *)
            ppat_list ~loc
              (List.map
                 (fun (p, e) ->
                   let p = compile_pat env p in
                   let e = compile env e in
                   ppat_tuple ~loc
                     [ ppat_any ~loc;         (* Binder wildcard - no syntax to specify *)
                       p;                     (* Pattern *)
                       e                      (* Result expression *)
                     ])
                 bs);
          ]
          
    (* Record literals: { field1: expr1, field2: expr2, ... } *)
    | Record es ->
        ppat_cons_one ~loc "EFlat"
          (ppat_list ~loc
             (List.map
                (fun (f, e) ->
                  ppat_tuple ~loc 
                    [ ppat_cons_one' ~loc "Some" (ppat_string ~loc f);  (* Field name *)
                      compile env e                                     (* Field value *)
                    ])
                es))
                
    (* Integer literals: 42 *)
    | Int i ->
        ppat_cons_many ~loc "EConstant"
          [
            ppat_any ~loc;                    (* Width wildcard - no syntax to specify *)
            ppat_string ~loc (string_of_int i);
          ]
          
    (* Qualified names: std::vec::Vec *)
    | Qualified path -> 
        ppat_cons_one ~loc "EQualified" (compile_path env path)
        
    (* Bound variables: x (resolves to De Bruijn index) *)
    | BoundVar s ->
        let i = find env s in              (* Look up variable in environment *)
        ppat_cons_one ~loc "EBound" (ppat_int ~loc i)
        
    (* Control flow *)
    | Break -> ppat_cons_zero ~loc "EBreak"
    | Bool b -> ppat_cons_one ~loc "EBool" (ppat_bool ~loc b)

  (* =================================================================================================
     PATH COMPILATION: Module and Qualified Names
     ================================================================================================= *)
     
  (* Compile qualified paths like std::vec::Vec to patterns.
     
     TARGET AST STRUCTURE: Paths are represented as (module_components, final_name) tuples
     Example: std::vec::Vec → (["std"; "vec"], "Vec")
     
     This allows pattern matching against both the module path and the final identifier.
   *)
  and compile_path env (pt : ParseTree.path) =
    let m, n =
      match List.rev pt with
      | n :: m -> List.rev m, n     (* Split path into module components and final name *)
      | _ -> failwith "impossible"   (* Paths must have at least one component *)
    in
    ppat_tuple ~loc 
      [ ppat_list ~loc (List.map (compile_path_item env) m);  (* Module path components *)
        compile_path_item env n                               (* Final name *)
      ]
      
  (* Compile individual path components (identifiers, wildcards, variables) *)
  and compile_path_item _env (pt : ParseTree.path_item) =
    match pt with
    | Wild -> ppat_any ~loc                      (* _ wildcard matches any component *)
    | Name s -> ppat_string ~loc s              (* Concrete identifier *)
    | Var txt -> ppat_var ~loc { txt; loc }     (* Pattern variable captures component *)

  (* =================================================================================================
     TYPE COMPILATION: Generic Types and Type Applications
     ================================================================================================= *)
     
  (* Compile type expressions to patterns for matching against type annotations.
     
     NOTE: Type compilation is simpler than expression compilation because types
     have less complex structure and fewer edge cases to handle.
   *)
  and _compile_typ env pt = compile_with_var env pt compile_pre_typ
  and compile_typ_list_pattern env (es : ParseTree.typ list) =
    compile_list_pattern env es compile_pre_typ
  
  (* Compile concrete type expressions *)  
  and compile_pre_typ env (pt : ParseTree.pre_typ) =
    match pt with
    
    (* Qualified type name: std::vec::Vec *)
    | TQualified path -> 
        ppat_cons_one' ~loc "TQualified" (compile_path env path)
        
    (* Generic type application: Vec<i32, String> *)
    | TApp (Fixed (TQualified p), ts) ->
        ppat_cons_many' ~loc "TApp" 
          [ compile_path env p; compile_typ_list_pattern env ts ]
          
    (* Type application with pattern variable: ?T<i32> *)  
    | TApp (PatternVar p, ts) ->
        ppat_cons_many' ~loc "TApp"
          [ ppat_var ~loc { txt = p; loc }; compile_typ_list_pattern env ts ]
          
    (* Invalid type application (caught at compile time) *)
    | TApp (_, _) -> 
        failwith "incorrect type application left-hand side"

  (* =================================================================================================
     PATTERN COMPILATION: Constructor Patterns for Match Arms  
     ================================================================================================= *)
     
  (* Compile pattern expressions that appear in match arms *)
  and compile_pat env pt = compile_with_var env pt compile_pre_pat
  and compile_pat_list_pattern env (es : ParseTree.pat list) =
    compile_list_pattern env es compile_pre_pat
    
  (* Compile concrete constructor patterns *)
  and compile_pre_pat env (pt : ParseTree.pre_pat) =
    match pt with
    
    (* Constructor pattern: Some(x), None, Cons(head, tail) *)
    | Cons (cons, ps) ->
        ppat_cons_many ~loc "PCons" 
          [ ppat_string ~loc cons;                    (* Constructor name *)
            compile_pat_list_pattern env ps           (* Constructor arguments *)
          ]
  
  in

  (* =================================================================================================
     COMPILATION ENTRY POINT
     ================================================================================================= *)
  
  (* Start compilation with the provided environment and parse tree *)
  compile env pt

(* =================================================================================================
   PPX INTEGRATION: Embedding Cremepat in OCaml Source Code
   ================================================================================================= *)

(* Main expansion function: transforms cremepat syntax to OCaml patterns.
   
   PROCESS:
   1. Parse the string payload to get ParseTree AST
   2. Extract source location from PPX context for error reporting
   3. Compile ParseTree to OCaml pattern AST using empty variable environment  
   4. Return the generated pattern for integration into source code
   
   USAGE: [%cremepat {| let ?x = ?init; ?body |}] becomes an OCaml pattern
 *)
let expand ~ctxt (payload : string) =
  let pt = parse payload in                                           (* Parse cremepat syntax *)
  let loc = Expansion_context.Extension.extension_point_loc ctxt in   (* Get source location *)
  compile_parse_tree empty loc pt                                     (* Compile to OCaml pattern *)

(* PPX Extension Declaration: Registers cremepat with the OCaml compiler.
   
   PARAMETERS:
   - "cremepat": The extension name (used in [%cremepat ...])
   - Pattern: Declares this extension produces patterns (not expressions or types)
   - single_expr_payload (estring __): Expects a single string literal as payload
   - expand: The function to call when this extension is encountered
   
   INTEGRATION: This creates the bridge between OCaml's PPX system and cremepat
 *)
let my_extension =
  Extension.V3.declare "cremepat" Pattern Ast_pattern.(single_expr_payload (estring __)) expand

(* Create PPX rule and register with the compiler driver *)  
let rule = Ppxlib.Context_free.Rule.extension my_extension
let () = Driver.register_transformation ~rules:[ rule ] "cremepat"

(* =================================================================================================
   CREMEPAT SUMMARY: A Complete PPX Architecture
   =================================================================================================
   
   CONGRATULATIONS! You've now seen a complete, working PPX extension. Here's what we learned:
   
   1. LEXICAL ANALYSIS (Lex.ml):
      - Convert text to tokens using sedlex regular expressions
      - Handle unification variables (?x, ?x..) as distinct token types
      - Provide location tracking for error reporting
      
   2. PARSING (Parse.mly):  
      - Define grammar rules using Menhir parser generator
      - Handle precedence through rule ordering and structure
      - Enable parameterized rules (with_vars) for code reuse
      - Support complex syntax (generics, paths, patterns)
      
   3. AST REPRESENTATION (ParseTree.ml):
      - Define recursive data types for language constructs
      - Use 'with_vars wrapper to enable unification variables
      - Mirror the structure of target AST for easy compilation
      
   4. COMPILATION (cremepat.ml):
      - Transform ParseTree to OCaml pattern AST using Ppxlib
      - Manage variable environments and De Bruijn indices  
      - Handle mutual recursion between expressions, patterns, types
      - Generate patterns that match target AST structure
      
   5. PPX INTEGRATION:
      - Register extension with OCaml compiler using Ppxlib
      - Handle string payloads and source location tracking
      - Integrate generated patterns into user code seamlessly
      
   This architecture demonstrates sophisticated compiler techniques:
   - Recursive descent parsing with precedence handling
   - Environment-based name resolution  
   - AST transformations with preservation of source locations
   - Integration with existing toolchain (OCaml compiler, Ppxlib)
   
   The key insight is that PPX extensions are mini-compilers that transform
   domain-specific syntax into standard OCaml constructs, enabling powerful
   metaprogramming while maintaining OCaml's type safety and tooling support.
 *)
