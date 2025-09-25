# Cremepat Expression Extension Design

## Overview

This document outlines the design for extending cremepat to support expression generation, not just pattern matching. The key challenge is balancing expressivity, convenience, and implementation complexity while handling type inference appropriately.

## Design Philosophy

### Core Principles

1. **Minimal Type Annotations**: Users should only need to annotate where absolutely necessary
2. **Gradual Typing**: Support both typed and untyped expression fragments  
3. **Target AST Compatibility**: Generate expressions that match Krml.Ast structure
4. **Incremental Implementation**: Build complexity gradually with clear phases

### Language Design Goals

- **Expressivity**: Support complex expressions with generics, method calls, etc.
- **Convenience**: Readable syntax similar to Rust/OCaml
- **Type Safety**: Catch type errors at cremepat compile time when possible
- **Flexibility**: Allow type inference where feasible, require annotations where necessary

## Language Syntax Design

### Basic Expression Syntax

```ocaml
(* Simple expressions - no type annotations needed *)
[%cremeexpr {| 42 |}]
[%cremeexpr {| true |}] 
[%cremeexpr {| x |}]

(* Function calls *)
[%cremeexpr {| f(a, b, c) |}]
[%cremeexpr {| std::vec::Vec::new() |}]

(* With type parameters *)
[%cremeexpr {| Vec::<i32>::new() |}]
[%cremeexpr {| f::<T, U>(x, y) |}]
```

### Type Annotation Strategy

```ocaml
(* Explicit type ascription when needed *)
[%cremeexpr {| (x : i32) |}]
[%cremeexpr {| (Vec::new() : Vec<i32>) |}]

(* Let bindings with optional type annotations *)
[%cremeexpr {| let x = 42; x + 1 |}]                    (* infer x : i32 *)
[%cremeexpr {| let x : u64 = 42; x + 1 |}]             (* explicit type *)
[%cremeexpr {| let x = Vec::new(); x.push(42) |}]       (* needs more context *)
[%cremeexpr {| let x : Vec<i32> = Vec::new(); x.push(42) |}]  (* explicit *)
```

### Pattern Variables in Expressions

```ocaml
(* Mix concrete and pattern parts *)
[%cremeexpr {| let x = ?init_expr; x + 42 |}]
[%cremeexpr {| f(?arg1, 42, ?arg2) |}]
[%cremeexpr {| Vec::<i32>::with_capacity(?size) |}]

(* List variables for argument lists *)
[%cremeexpr {| f(?first_arg, ?rest_args..) |}]
```

### Advanced Features

```ocaml
(* Method calls and field access *)
[%cremeexpr {| obj.method(args) |}]
[%cremeexpr {| obj.field |}]

(* Complex generics *)
[%cremeexpr {| HashMap::<String, Vec<i32>>::new() |}]

(* Control flow *)
[%cremeexpr {| if cond { expr1 } else { expr2 } |}]
[%cremeexpr {| match x { pattern => expr, ... } |}]
```

## Type System Design

### Type Environment Structure

```ocaml
type type_env = {
  (* Variable bindings with their types *)
  vars : (string * typ) list;
  
  (* Type variables in scope (for generics) *)
  type_vars : string list;
  
  (* Const generic variables *)
  const_generics : (string * typ) list;
  
  (* Context for type inference *)
  expected_type : typ option;
  
  (* Function signatures available *)
  functions : (lident * type_scheme) list;
}
```

### Type Inference Strategy

1. **Bottom-Up Inference**: Start from literals and variables, propagate upward
2. **Top-Down Constraints**: Use expected types to constrain inference
3. **Unification**: Resolve type variables through constraint solving
4. **Fallback to Annotations**: Require explicit types when inference fails

### Type Annotation Levels

```ocaml
type annotation_level = 
  | Explicit of typ           (* User provided type *)
  | Inferred of typ          (* Successfully inferred *)
  | Unknown                  (* Needs more information *)
  | Error of string         (* Type error *)
```

## Implementation Architecture

### File Structure

```
cremepat/
├── ParseTree.ml              (* Current - patterns only *)
├── ExprParseTree.ml          (* New - expression AST *)
├── ExprLex.ml                (* New - expression-specific lexing *)
├── ExprParse.mly             (* New - expression grammar *)
├── TypeInference.ml          (* New - type inference engine *)
├── ExprCompiler.ml           (* New - expression compilation *)
├── ExprExtension.ml          (* New - PPX integration *)
└── EXPRESSION_DESIGN.md      (* This file *)
```

### Implementation Phases

#### Phase 1: Basic Expressions (Weeks 1-2)
- Literals (integers, booleans, strings)  
- Variables and qualified names
- Simple function calls without generics
- Basic type inference for literals
- PPX integration for `[%cremeexpr ...]`

**Success Criteria:**
```ocaml
[%cremeexpr {| 42 |}]           (* → with_type TInt (EConstant "42") *)
[%cremeexpr {| x |}]            (* → with_type ?T (EBound i) *)
[%cremeexpr {| f(a, b) |}]      (* → with_type ?T (EApp (f, [a; b])) *)
```

#### Phase 2: Let Bindings and Type Propagation (Weeks 3-4)
- Let expressions with type inference
- Environment management
- Type constraint propagation
- Pattern variables in expressions

**Success Criteria:**
```ocaml
[%cremeexpr {| let x = 42; x + 1 |}]                    (* infer i32 *)
[%cremeexpr {| let x = ?init; x + 1 |}]                 (* mixed concrete/pattern *)
```

#### Phase 3: Generics and Advanced Types (Weeks 5-6)
- Type parameters in function calls
- Generic type instantiation
- Const generic evaluation
- Method call resolution

**Success Criteria:**
```ocaml
[%cremeexpr {| Vec::<i32>::new() |}]                    (* generic instantiation *)
[%cremeexpr {| HashMap::<K, V>::with_capacity(?n) |}]   (* complex generics *)
```

#### Phase 4: Control Flow and Pattern Matching (Weeks 7-8)
- If expressions
- Match expressions  
- While loops
- Integration with existing pattern system

## Technical Challenges and Solutions

### Challenge 1: Type Inference Complexity

**Problem**: Full Hindley-Milner inference is complex and may not fit well with Rust's type system.

**Solution**: Implement local type inference with explicit annotations as fallback:
```ocaml
let infer_expr env expr =
  match expr with
  | Literal _ -> infer_literal_type expr
  | Var name -> lookup_var_type env name
  | App (f, args) -> 
      (* Try to infer from function signature *)
      match lookup_function_type env f with
      | Some scheme -> instantiate_and_apply scheme args
      | None -> require_annotation expr
```

### Challenge 2: Const Generic Evaluation

**Problem**: Const generics need compile-time evaluation, which is complex.

**Solution**: Support limited const generic expressions initially:
```ocaml
type const_generic_value = 
  | CInt of int
  | CBool of bool
  | CUnevaluated of expr  (* For complex cases *)

let eval_const_generic env = function
  | Fixed (Int n) -> CInt n
  | Fixed (Bool b) -> CBool b
  | PatternVar name -> lookup_const_generic env name
  | _ -> CUnevaluated expr  (* Defer to runtime *)
```

### Challenge 3: Method Resolution

**Problem**: Rust method calls require trait resolution, which is very complex.

**Solution**: Phase the implementation:
1. **Phase 1**: Only inherent methods (methods defined on types directly)
2. **Phase 2**: Simple trait methods with explicit disambiguation  
3. **Phase 3**: Full trait resolution (if needed)

### Challenge 4: Integration with Existing Patterns

**Problem**: Expressions and patterns need to work together seamlessly.

**Solution**: Share common infrastructure:
```ocaml
(* Shared type definitions *)
type 'a with_type_info = {
  node : 'a;
  typ : typ option;
  inferred_typ : typ option;
}

(* Common compilation environment *)
type unified_env = {
  pattern_env : PatternEnv.t;
  expr_env : ExprEnv.t;
  shared_types : typ list;
}
```

## Error Handling Strategy

### Compile-Time Errors

1. **Type Mismatch**: Clear error messages with suggestions
2. **Unresolved Names**: Suggest similar names or missing imports
3. **Generic Instantiation Failures**: Show expected vs provided parameters
4. **Pattern/Expression Mixing**: Explain where each is allowed

### Runtime Fallbacks

1. **Unknown Types**: Generate code with type wildcards and runtime checks
2. **Complex Const Generics**: Defer evaluation to runtime
3. **Unresolved Methods**: Generate dynamic dispatch code

## Usage Examples

### Simple Use Cases

```ocaml
(* Replace complex manual expression building *)
let mk_vec_new t =
  with_type (mk_vec t) (EApp (
    with_type (TArrow ([t], mk_vec t)) (ETApp (
      with_type (TScheme (["T"], TArrow ([TBound 0], TApp (vec, [TBound 0])))) 
        (EQualified (["std"; "vec"], "Vec")),
      [], [], [t])),
    []))

(* With cremepat expressions *)
let mk_vec_new t = [%cremeexpr {| Vec::<{typ t}>::new() |}]
```

### Advanced Use Cases

```ocaml
(* Complex expression building *)
let mk_range_loop start end_ body =
  [%cremeexpr {| 
    for i in {expr start}..{expr end_} {
      {expr body}
    }
  |}]

(* With pattern integration *)
match expr with
| [%cremepat {| for ?var in ?start..?end_ { ?body } |}] ->
    let optimised_body = optimize_loop_body body in
    [%cremeexpr {| 
      let mut i = {expr start};
      while i < {expr end_} {
        {expr optimised_body};
        i += 1
      }
    |}]
```

## Testing Strategy

### Unit Tests
- Type inference for each expression type
- Error handling for malformed inputs
- Integration with existing pattern system

### Integration Tests  
- Real-world expression generation
- Performance comparison with manual building
- Compatibility with existing codebase

### Property-Based Tests
- Generated expressions should be well-typed
- Round-trip: parse → compile → pretty-print → parse
- Type inference should be deterministic

## Risk Assessment

### High Risk
- **Type inference complexity**: May need significant simplification
- **Const generic evaluation**: Very complex in full generality
- **Method resolution**: Rust's trait system is complex

### Medium Risk  
- **PPX integration**: New extension points may conflict
- **Performance**: Expression compilation may be slow
- **Error messages**: Hard to make user-friendly

### Low Risk
- **Basic expression generation**: Well-understood problem
- **Pattern integration**: Existing infrastructure helps
- **Incremental implementation**: Can ship useful subsets

## Success Metrics

### Phase 1 Success
- Can generate basic expressions (literals, variables, simple calls)
- Type inference works for simple cases
- PPX integration functional
- 80% of basic use cases covered

### Phase 2 Success  
- Let bindings with type inference
- Pattern variables in expressions
- Error handling with good messages
- 90% of medium complexity use cases

### Phase 3 Success
- Generics and type parameters
- Most const generic cases
- Method calls (inherent methods)
- 95% of complex use cases

### Full Success
- Complete feature parity with manual expression building
- Better ergonomics than manual approach
- Integrated into eurydice codebase
- Documentation and examples complete

## Development Timeline

- **Week 1**: ExprParseTree.ml, basic lexing/parsing
- **Week 2**: Basic type inference, PPX integration  
- **Week 3**: Let bindings, environment management
- **Week 4**: Pattern variables, mixed expressions
- **Week 5**: Type parameters, basic generics
- **Week 6**: Const generics, complex types
- **Week 7**: Control flow, method calls
- **Week 8**: Integration, documentation, testing

This design provides a clear path forward while acknowledging the significant challenges involved in implementing a full expression extension to cremepat.