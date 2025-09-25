# Cremepat Function Definition Extension

## Quick Start

This extension adds support for defining Rust-like function syntax in cremepat, making builtin function definitions more readable and maintainable.

### Before (Traditional Approach)
```ocaml
let array_to_slice = 
  let t = TBound 0 in
  fun lid ->
    DFunction (
      None, [ Private ], 0, 1, TBuf (t, false), lid,
      [ Krml.Helpers.fresh_binder "arr" (TArray (t, _)) ],
      (* Complex manual AST construction... *)
    )
```

### After (New Extension)
```ocaml
let array_to_slice = 
  [%cremepat_fun_def {|
fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
  SliceRef<T> { pointer = arr, length = N }
} |}]
```

## Installation & Setup

1. The extension is automatically available when using cremepat
2. No additional dependencies required beyond existing cremepat setup
3. Compatible with existing cremepat usage

## Usage Guide

### Basic Syntax

```
[%cremepat_fun_def {|
fn function_name<TypeParams><ConstParams>(parameters) -> ReturnType {
  body_expression
} |}]
```

### Components Explained

- **`fn`**: Function keyword (required)
- **`function_name`**: Identifier for the function
- **`<TypeParams>`**: Optional type parameters like `<T, U>`
- **`<ConstParams>`**: Optional const generics like `<N : size_t>`
- **`(parameters)`**: Function parameters as `name: type` pairs
- **`-> ReturnType`**: Return type specification
- **`{ body }`**: Function body as an expression

### Examples

#### 1. Simple Function
```ocaml
[%cremepat_fun_def {|
fn add(a: i32, b: i32) -> i32 {
  a + b
} |}]
```

#### 2. Generic Function
```ocaml
[%cremepat_fun_def {|
fn identity<T>(x: T) -> T {
  x
} |}]
```

#### 3. Array Function with Const Generics
```ocaml
[%cremepat_fun_def {|
fn array_repeat<T><N : size_t>(value: T) -> [T; N] {
  [value; N]
} |}]
```

#### 4. Complex Example (Original Request)
```ocaml
[%cremepat_fun_def {|
fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
  SliceRef<T> { pointer = arr, length = N }
} |}]
```

## How It Works

The extension parses the Rust-like syntax and generates OCaml patterns that match Krml's `DFunction` constructor:

```ocaml
DFunction (
  _,                    (* comment - any *)
  _,                    (* flags - any *)
  n_const_generics,     (* number of const generics *)
  n_type_params,        (* number of type parameters *)
  return_type_pattern,  (* compiled return type *)
  (_, function_name),   (* location identifier *)
  parameter_patterns,   (* compiled parameters *)
  body_pattern         (* compiled body expression *)
)
```

## Pattern Variables

You can use cremepat pattern variables in function definitions:

```ocaml
[%cremepat_fun_def {|
fn ?function_name<T>(param: ?param_type) -> ?return_type {
  ?body_expr
} |}]
```

This generates patterns that can match any function with the specified structure.

## Integration with Existing Code

The new extension works alongside existing cremepat usage:

```ocaml
match ast_node with
| [%cremepat {| let x = ?e1; ?e2 |}] -> 
    handle_let_binding e1 e2
    
| [%cremepat_fun_def {|
fn ?name<T>(param: T) -> T {
  ?body
} |}] ->
    handle_function_definition name body
    
| _ -> handle_other_cases ()
```

## Testing

### Running Tests

```bash
# Basic functionality test
cd eurydice
ocaml test/test_cremepat_simple.ml

# Full parsing test (requires dependencies)
ocaml test/test_cremepat_functions.ml

# Usage demonstration
ocaml test/demo_cremepat_usage.ml
```

### Validating Your Implementation

1. **Syntax Check**: Run `menhir --only-preprocess cremepat/Parse.mly` to verify grammar
2. **Basic Test**: Run the simple test to verify structure
3. **Integration Test**: Use in actual Eurydice builtin definitions

## File Structure

```
cremepat/
├── ParseTree.ml          # Extended with function definition AST
├── Parse.mly            # Extended with function grammar rules  
├── Lex.ml               # Extended with 'fn' keyword
├── cremepat.ml          # Extended with function compilation logic
├── FUNCTION_DEFINITIONS.md  # Detailed implementation docs
└── README.md            # Original cremepat docs
```

## Implementation Details

### Files Modified

1. **ParseTree.ml**: Added `pre_function_def` type and `FunctionDef` variant
2. **Parse.mly**: Added grammar rules for `fn` syntax and function components
3. **Lex.ml**: Added `fn` keyword to lexer keywords
4. **cremepat.ml**: Added compilation logic and new extension declaration

### Design Decisions

- **Separate Extension**: Uses `cremepat_fun_def` to avoid breaking existing usage
- **Rust-like Syntax**: Familiar syntax for easier adoption
- **Pattern Integration**: Full integration with existing pattern variable system
- **AST Mapping**: Direct mapping to Krml's `DFunction` structure

## Backwards Compatibility

- ✅ Existing `cremepat` usage unchanged
- ✅ No breaking changes to current patterns  
- ✅ New extension is completely separate
- ✅ Can be used alongside existing patterns

## Limitations & Future Work

### Current Limitations
- Simplified type syntax (more Rust types can be added)
- Basic parameter handling (pattern parameters not yet supported)
- No trait bounds or where clauses

### Future Enhancements
- Advanced Rust type syntax (lifetimes, trait objects, etc.)
- Pattern matching in function parameters
- Multiple function definitions in one extension
- Generic constraints and where clauses

## Troubleshooting

### Common Issues

1. **Parse Error**: Check that braces `{}` are balanced and syntax is correct
2. **Missing Dependencies**: Some tests require full Eurydice build environment
3. **Pattern Matching**: Ensure pattern variables use `?` prefix

### Getting Help

1. Check existing cremepat documentation in `cremepat/README.md`
2. Look at usage examples in `lib/Cleanup2.ml`
3. Run the demo script to see expected output

## Contributing

To extend this functionality:

1. Add new syntax to `Parse.mly` 
2. Extend AST types in `ParseTree.ml`
3. Add compilation logic in `cremepat.ml`
4. Update tests and documentation
5. Ensure backwards compatibility

---

**Status**: Implementation complete and ready for use! 🎉