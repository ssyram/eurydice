# Cremepat Builtin Expression Extension: 1-Week Minimal Implementation Plan

## Objective

Create a minimal but functional cremepat expression extension focused **only** on builtin functions from `Builtin.ml`. This approach dramatically reduces complexity while providing immediate value.

## Scope Limitation: Builtin Functions Only

Instead of implementing a full expression language, we focus exclusively on generating calls to the builtin functions already defined in `lib/Builtin.ml` such as:

- `slice_index`, `array_to_slice`, `discriminant`  
- Vector operations, slice operations
- 128-bit integer operations
- Range iterators

**Key Insight**: These functions have **well-defined types already**, eliminating the need for complex type inference.

## Minimal Language Design

### Supported Syntax (Week 1 Only)

```ocaml
(* Simple builtin calls *)
[%cremebuiltin {| slice_index(?slice, ?index) |}]
[%cremebuiltin {| array_to_slice(?array) |}]
[%cremebuiltin {| discriminant<?T, ?U>(?value) |}]

(* With explicit types when needed *)
[%cremebuiltin {| slice_index::<i32>(?slice, ?index) |}]
```

**No support for**:
- Let bindings
- Control flow (if/match/while)
- Complex expressions
- Type inference beyond builtin signatures
- Variables or complex paths

## Architecture: Reuse Maximum, Build Minimum

### Files to Create (3 small files)

1. **`BuiltinExpr.ml`** (~150 lines)
   - Simple AST for builtin calls only
   - Direct mapping to `Builtin.ml` functions

2. **`BuiltinParser.ml`** (~100 lines)  
   - Simple recursive descent parser
   - Parse only: `function_name(args)` and `function_name::<types>(args)`

3. **`BuiltinExtension.ml`** (~80 lines)
   - PPX integration for `[%cremebuiltin ...]`
   - Direct compilation to Krml expressions

**Total**: ~330 lines of new code

### No Complex Infrastructure Needed

- **No lexer**: Use simple string splitting and regex
- **No grammar**: Simple recursive descent parsing  
- **No type inference**: Lookup builtin signatures directly
- **No environment management**: Pattern variables only

## Implementation Details

### Phase 1: Core Structure (Day 1-2)

```ocaml
(* BuiltinExpr.ml - Minimal AST *)
type builtin_call = {
  name : string;                    (* e.g., "slice_index" *)
  type_args : string list;          (* e.g., ["i32"] *)
  args : arg list;                  
}

and arg = 
  | PatternVar of string            (* ?x *)
  | Concrete of string              (* literal - minimal support *)
```

### Phase 2: Simple Parser (Day 3-4)

```ocaml
(* BuiltinParser.ml - No complex parsing *)
let parse_builtin_call input =
  (* Parse: "function_name::<T>(arg1, arg2)" *)
  let function_name, type_args, args = parse_simple_call input in
  { name = function_name; type_args; args }
```

### Phase 3: Direct Compilation (Day 5-6)

```ocaml
(* BuiltinExtension.ml - Direct to Krml *)
let compile_builtin_call call =
  (* Lookup in Builtin.ml *)
  let builtin = lookup_builtin call.name in
  (* Generate Krml expression directly *)
  generate_krml_call builtin call.args
```

### Phase 4: Testing & Integration (Day 7)

- Unit tests for each builtin function
- Integration with existing cremepat  
- Documentation and examples

## AI vs Human Effort Analysis

### AI Can Handle (95% of implementation):
- **Parsing logic**: Simple string manipulation and regex
- **AST definitions**: Straightforward data structures  
- **Code generation**: Template-based generation to Krml
- **Builtin lookup**: Dictionary-style mapping
- **Testing**: Property-based tests for each builtin
- **Documentation**: Usage examples and API docs

### Human Required (5% of implementation):
- **PPX integration points**: Ensuring compatibility with existing system
- **Error message design**: Making error messages helpful for users
- **Integration testing**: Testing with real eurydice codebase
- **Performance validation**: Ensuring no compilation slowdown

## Concrete Benefits After 1 Week

### Immediate Usability
```ocaml
(* Instead of complex manual construction *)
let build_slice_access slice index =
  with_type element_type (EApp (
    with_type (TArrow ([slice_type; index_type], element_type))
      (EQualified (["Eurydice"], "slice_index")),
    [slice_expr; index_expr]))

(* Simply write *)
let build_slice_access slice index =
  [%cremebuiltin {| slice_index(?slice, ?index) |}]
```

### Coverage of Common Cases
- **80%** of builtin function usage covered
- **Immediate productivity gain** for developers
- **Foundation** for future expression extensions

## Implementation Timeline

| Day | Task | Effort | AI/Human |
|-----|------|--------|----------|
| 1 | BuiltinExpr.ml AST | 4 hours | 100% AI |
| 2 | BuiltinParser.ml basic parsing | 6 hours | 90% AI |
| 3 | BuiltinExtension.ml PPX integration | 8 hours | 80% AI |
| 4 | Builtin lookup and validation | 6 hours | 95% AI |
| 5 | Code generation to Krml | 8 hours | 90% AI |
| 6 | Testing and debugging | 6 hours | 70% AI |
| 7 | Integration and documentation | 4 hours | 60% AI |

**Total**: 42 hours over 7 days, mostly AI-assisted

## Risk Assessment

### Low Risk Areas
- **Parsing**: Simple syntax, well-defined scope
- **Code generation**: Direct mapping to existing builtins
- **Testing**: Each builtin is independent

### Medium Risk Areas  
- **PPX integration**: May need adjustment for existing system
- **Error handling**: Making user-friendly error messages

### High Risk Areas
- **None** - scope is intentionally minimal

## Success Metrics

### Technical Success
- [ ] All builtin functions from `Builtin.ml` supported
- [ ] Compilation time <200ms for typical usage
- [ ] Zero regressions in existing cremepat functionality
- [ ] 90%+ test coverage

### User Success
- [ ] 50% reduction in code for builtin function usage
- [ ] Clear error messages for 95% of common mistakes
- [ ] Documentation with examples for each builtin

## Extension Path (Future Weeks)

Week 2+: Add support for:
- Simple let bindings: `let x = builtin(); builtin2(x)`
- Basic control flow: `if cond { builtin1() } else { builtin2() }`
- Variable references beyond pattern variables

This minimal approach provides immediate value while establishing the foundation for more complex features.

## Why This Approach Works

1. **Leverages existing infrastructure**: All type information already in `Builtin.ml`
2. **Minimizes complexity**: No type inference, no complex parsing
3. **Provides immediate value**: Covers most common use cases
4. **AI-friendly**: Mostly template-based code generation
5. **Extensible**: Clear path to add more features incrementally

This plan transforms a 16-week complex project into a 1-week focused implementation that delivers 80% of the value with 20% of the effort.