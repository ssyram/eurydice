# Cremepat PPX Learning Guide: From Beginner to Expert

This comprehensive guide teaches you how to understand and extend the `cremepat` PPX extension. Follow this systematic learning path to master PPX development and pattern matching compilation.

## Table of Contents

1. [What is Cremepat?](#what-is-cremepat)
2. [PPX Architecture Overview](#ppx-architecture-overview)
3. [Learning Path: File by File](#learning-path-file-by-file)
4. [Core Concepts Deep Dive](#core-concepts-deep-dive)
5. [Implementing Expression Extensions](#implementing-expression-extensions)
6. [Practical Examples](#practical-examples)
7. [Advanced Topics](#advanced-topics)
8. [Common Pitfalls and Solutions](#common-pitfalls-and-solutions)

## What is Cremepat?

Cremepat (CREdible Meta-Extensible PATterns) is a PPX extension that allows you to write readable pattern matching syntax instead of complex nested OCaml patterns when working with AST nodes.

### Before Cremepat:
```ocaml
match expr with
| { node = ELet (_, { node = EBound i; _ }, { node = ESequence es; _ }); _ } ->
    (* Complex nested pattern matching *)
| { node = EApp ({ node = ETApp ({ node = EQualified (["std"; "vec"], "Vec"); _ }, _, _, _); _ }, args); _ } ->
    (* Even more complex patterns *)
```

### After Cremepat:
```ocaml
match expr with
| [%cremepat {| let ?x = ?init; ?body |}] ->
    (* Readable, intuitive pattern *)
| [%cremepat {| std::vec::Vec<?T>(?args..) |}] ->
    (* Clear intent and structure *)
```

## PPX Architecture Overview

PPX extensions are mini-compilers that transform domain-specific syntax into standard OCaml constructs. The cremepat architecture follows this pipeline:

```
Input String → Lexer → Parser → AST → Compiler → OCaml Pattern AST → Integration
     ↓            ↓        ↓       ↓         ↓              ↓              ↓
   "{|...|}"   → Tokens → ParseTree → Analysis → Pattern → Ppxlib → Source Code
```

### Key Components:

1. **Lexer** (`Lex.ml`): Converts text to tokens
2. **Parser** (`Parse.mly`): Builds abstract syntax tree
3. **AST** (`ParseTree.ml`): Defines data structures
4. **Compiler** (`cremepat.ml`): Transforms AST to OCaml patterns
5. **Integration**: Ppxlib handles embedding in source code

## Learning Path: File by File

Follow this order for optimal understanding:

### 1. Start with ParseTree.ml
**Why first?** Understanding the data structures is crucial before learning how they're created or used.

**Key concepts:**
- `with_vars` wrapper type
- Expression, pattern, and type representations
- Unification variables vs. concrete values

**Study questions:**
- How does `PatternVar` differ from `ListPatternVar`?
- Why is `expr = pre_expr with_vars`?
- What's the relationship between expressions and patterns?

### 2. Move to Lex.ml
**Why second?** Learn how text becomes structured tokens.

**Key concepts:**
- Regular expression patterns
- Token types and their meanings
- Special handling of unification variables (`?x`, `?x..`)

**Study questions:**
- How are keywords distinguished from identifiers?
- Why are `UVAR` and `UVARLIST` separate token types?
- How does location tracking work?

### 3. Study Parse.mly
**Why third?** Understand how tokens become structured AST.

**Key concepts:**
- Grammar rules and precedence
- Parameterized rules (`with_vars`, `iseparated_twoplus_list`)
- Expression precedence hierarchy

**Study questions:**
- How does precedence work without `%prec` declarations?
- Why is `with_vars(X)` parameterized?
- How do the different expression levels interact?

### 4. Master cremepat.ml
**Why last?** The most complex part - compilation to OCaml patterns.

**Key concepts:**
- Environment management and De Bruijn indices
- Mutual recursion between compilation functions
- Pattern AST construction
- PPX integration

**Study questions:**
- How are bound variables resolved to indices?
- Why the mutual recursion between expressions, patterns, and types?
- How does the `ppat_node` wrapper work?

## Core Concepts Deep Dive

### Unification Variables: The Heart of Cremepat

Unification variables are what make cremepat powerful. They come in two flavors:

```ocaml
type 'a with_vars = 
  | Fixed of 'a              (* Concrete value: 42, "hello", Vec *)
  | PatternVar of string     (* Single capture: ?x *)
  | ListPatternVar of string (* List capture: ?args.. *)
```

**Usage rules:**
- `?x` captures single elements
- `?x..` captures lists (only in tail position)
- Can appear anywhere in the AST structure

### Environment Management

The environment tracks variable bindings using a simple list:

```ocaml
type env = string list  (* Most recent binding first *)
```

**Example:**
```
Pattern: let x = ?init in let y = x + ?val in y
Environment progression:
  []          → empty
  ["x"]       → after "let x"  
  ["y"; "x"]  → after "let y"
  
Variable resolution:
  "y" → index 0 (EBound 0)
  "x" → index 1 (EBound 1)
```

### Target AST Structure

Cremepat generates patterns matching this structure:
```ocaml
type expr = { node : expr_node; typ : typ; meta : meta }
```

Every pattern wraps the node content:
```ocaml
{ node = ELet (_, init_pattern, body_pattern); _ }
```

## Implementing Expression Extensions

Based on the cremepat architecture, here's a detailed roadmap for implementing expression extensions:

### Challenge: Type Inference

Unlike patterns, expressions must be well-typed. This requires:

1. **Type Environment Extension**:
```ocaml
type expr_env = {
  vars : (string * typ) list;        (* variable types *)
  type_vars : string list;           (* type variables *)
  const_generics : (string * typ) list; (* const generic types *)
}
```

2. **Type Inference Rules**:
```ocaml
let rec infer_type env expr =
  match expr with
  | Let (var, init, body) ->
      let init_ty = infer_type env init in
      let env' = extend_env env var init_ty in
      infer_type env' body
  | App { head; args; ts; _ } ->
      let head_ty = infer_type env head in
      let arg_tys = List.map (infer_type env) args in
      apply_function_type head_ty ts arg_tys
  (* ... *)
```

### Implementation Strategy

#### Phase 1: Basic Expressions
Start with simple, well-typed expressions:
- Variables and qualified names
- Function calls without generics
- Basic literals (integers, booleans)

#### Phase 2: Control Structures
Add control flow:
- Let bindings with type inference
- Conditional expressions
- Simple loops

#### Phase 3: Advanced Features  
Implement complex constructs:
- Generic function calls
- Const generic evaluation
- Method calls with trait resolution

### Recommended File Structure

```
cremepat/
├── ParseTree.ml     (* Add expr_with_type definitions *)
├── Parse.mly        (* Add expression grammar rules *)
├── ExprLex.ml       (* Optional: expression-specific lexer *)
├── ExprCompiler.ml  (* New: expression compilation logic *)
├── TypeInference.ml (* New: type inference engine *)
└── cremepat.ml      (* Integration point *)
```

### Example Extension Declaration

```ocaml
(* Expression extension alongside pattern extension *)
let expr_extension =
  Extension.V3.declare "cremeexpr" Expression 
    Ast_pattern.(single_expr_payload (estring __)) 
    expand_expression

let pattern_extension = 
  Extension.V3.declare "cremepat" Pattern
    Ast_pattern.(single_expr_payload (estring __))
    expand_pattern
```

## Practical Examples

### Example 1: Understanding Pattern Compilation

**Input:**
```ocaml
[%cremepat {| let ?x = Vec::new(); ?body |}]
```

**ParseTree AST:**
```ocaml
Fixed (Let ("x", 
  Fixed (App { head = Fixed (Qualified [Name "Vec"; Name "new"]); 
               args = []; ts = []; cgs = []; methods = [] }),
  PatternVar "body"))
```

**Generated OCaml Pattern:**
```ocaml
{ node = ELet (_, 
    { node = EApp (
        { node = ETApp ({ node = EQualified (["Vec"], "new"); _ }, [], [], []); _ }, 
        []); _ }, 
    body); _ }
```

### Example 2: Complex Application Pattern

**Input:**
```ocaml
[%cremepat {| std::vec::Vec::<i32>::with_capacity(?cap) |}]
```

**Key compilation steps:**
1. Parse path: `["std"; "vec"; "Vec"]`
2. Extract type args: `[TQualified ["i32"]]`
3. Generate ETApp pattern with type instantiation
4. Wrap in EApp with arguments

### Example 3: List Variables in Practice

**Input:**
```ocaml
[%cremepat {| func(?first, ?rest..) |}]
```

**Pattern matching behavior:**
- `?first` captures the first argument
- `?rest..` captures remaining arguments as a list
- Only works because arguments form a list structure

## Advanced Topics

### Const Generic Evaluation

For expression extensions, const generics need evaluation:

```ocaml
let eval_const_generic env = function
  | Fixed (Int n) -> ConstInt n
  | PatternVar name -> lookup_const_generic env name  
  | Fixed (BoundVar v) -> eval_bound_const_generic env v
  (* Complex evaluation rules *)
```

### Method Resolution

Rust-style method calls require trait information:

```ocaml
let resolve_method env receiver method_name =
  let receiver_ty = infer_type env receiver in
  let traits = find_applicable_traits env receiver_ty in
  List.find_map (lookup_method method_name) traits
```

### Lifetime Management

Expressions may need lifetime tracking:

```ocaml
type lifetime = Named of string | Anonymous | Static

type typ_with_lifetime = {
  base : typ;
  lifetimes : lifetime list;
}
```

## Common Pitfalls and Solutions

### Pitfall 1: List Variable Placement
**Problem:** Using `?x..` in non-tail position
**Solution:** Compiler validates list variable placement

### Pitfall 2: Environment Management
**Problem:** Forgetting to extend environment in let bindings
**Solution:** Always call `push env var` after processing binding

### Pitfall 3: Type Annotation Mismatches
**Problem:** Generated patterns don't match actual AST structure
**Solution:** Study target AST carefully, test with simple examples

### Pitfall 4: Precedence Issues
**Problem:** Grammar ambiguities causing parse errors
**Solution:** Use rule ordering and test extensively

## Development Workflow

### 1. Design Phase
- Define target syntax clearly
- Study existing AST structures
- Plan type inference strategy

### 2. Implementation Phase
- Start with minimal subset
- Add comprehensive tests
- Iterate on error messages

### 3. Testing Strategy
```ocaml
(* Unit tests for compilation *)
let test_let_pattern () =
  let input = {| let ?x = ?init; ?body |} in
  let pattern = parse_and_compile input in
  assert_matches_structure pattern expected_pattern

(* Integration tests with real code *)
let test_integration () =
  let code = [%cremepat {| Vec::new() |}] in
  (* Test in actual match expression *)
```

### 4. Documentation
- Comment every major function
- Provide usage examples  
- Explain design decisions
- Create troubleshooting guide

## Conclusion

Cremepat demonstrates sophisticated compiler techniques in a practical, useful tool. The key insights are:

1. **Domain-Specific Languages**: PPX enables embedding custom syntax
2. **Compiler Architecture**: Clean separation of concerns (lex/parse/compile)
3. **AST Transformations**: Systematic translation between representations
4. **Type-Directed Compilation**: Using type information to guide generation
5. **Metaprogramming**: Generating code that generates code

Master these concepts, and you'll be able to create powerful PPX extensions that make OCaml even more expressive and maintainable.

The jump from patterns to expressions is significant due to type inference requirements, but following the systematic approach outlined here will help you succeed. Start small, test extensively, and gradually build complexity.

Happy PPX hacking! 🚀