# Cremepat Function Definition Support

## Overview

This extension to cremepat adds support for function definitions using Rust-like syntax. It allows writing builtin functions in Eurydice using more human-readable syntax instead of manually constructing `DFunction` nodes.

## Syntax

The new `cremepat_fun_def` extension supports the following syntax:

```ocaml
[%cremepat_fun_def {|
fn function_name<TypeParam1, TypeParam2><ConstParam1 : type1>(param1: ptype1, param2: ptype2) -> ReturnType {
  function_body_expression
} |}]
```

### Components

- **`fn`**: Function keyword
- **`function_name`**: The name of the function
- **`<TypeParam1, TypeParam2>`**: Optional type parameters
- **`<ConstParam1 : type1>`**: Optional constant generic parameters (like array sizes)
- **`(param1: ptype1, param2: ptype2)`**: Function parameters with types
- **`-> ReturnType`**: Return type specification
- **`{ function_body_expression }`**: Function body as an expression

## Example

```ocaml
[%cremepat_fun_def {|
fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
  SliceRef<T> { pointer = arr, length = N }
} |}]
```

This generates a pattern that matches a `DFunction` node with:
- Function name: "array_to_slice"
- 1 type parameter (T)
- 1 const generic parameter (N : size_t)
- 1 regular parameter (arr : &[T; N])
- Return type: SliceRef<T>
- Body: SliceRef<T> { pointer = arr, length = N }

## Generated Pattern

The function definition is compiled to a pattern matching the Krml `DFunction` constructor:

```ocaml
DFunction (
  _,                    (* comment *)
  _,                    (* flags *)
  1,                    (* n_cgs - number of const generics *)
  1,                    (* n - number of type parameters *)
  SliceRef<T>,          (* return type *)
  (_, "array_to_slice"), (* lid - location identifier *)
  [_],                  (* binders for parameters *)
  body_pattern          (* compiled body expression *)
)
```

## Implementation Details

### Files Modified

1. **`cremepat/ParseTree.ml`**: Added `pre_function_def` and `function_def` types, extended `pre_expr` with `FunctionDef` variant
2. **`cremepat/Parse.mly`**: Added grammar rules for function definition syntax, new tokens
3. **`cremepat/Lex.ml`**: Added `fn` keyword to lexer
4. **`cremepat/cremepat.ml`**: Added compilation logic for function definitions, new extension declaration

### Key Design Decisions

1. **Separate Extension**: Uses `cremepat_fun_def` instead of extending the existing `cremepat` extension to avoid breaking existing pattern matching usage.

2. **AST Structure**: The function definition AST closely mirrors the structure needed for Krml's `DFunction` constructor, making compilation straightforward.

3. **Pattern Variables**: Supports the same pattern variable mechanism (`?var`) as regular cremepat expressions.

4. **Type Safety**: Leverages the existing `with_vars` mechanism to maintain type safety for pattern variables.

## Usage in Builtin Functions

This extension is particularly useful for defining builtin functions in `lib/Builtin.ml` with more readable syntax:

```ocaml
let array_to_slice = 
  [%cremepat_fun_def {|
fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
  SliceRef<T> { pointer = arr, length = N }
} |}]
```

Instead of manually constructing:

```ocaml
let array_to_slice = 
  fun lid ->
    DFunction (
      None, [ Private ], 0, 1, slice_ref_type,
      lid,
      [ fresh_binder "arr" array_type ],
      (* complex body construction *)
    )
```

## Testing

### Running Tests

1. **Simple syntax validation**:
   ```bash
   cd eurydice
   dune exec -- test/test_cremepat_simple.exe
   ```

2. **Full parsing test** (requires build dependencies):
   ```bash
   cd eurydice  
   dune exec -- test/test_cremepat_functions.exe
   ```

### Test Files

- `test/test_cremepat_simple.ml`: Basic syntax and structure validation
- `test/test_cremepat_functions.ml`: Full parsing functionality test

## Backwards Compatibility

The existing `cremepat` extension remains unchanged and fully compatible. The new `cremepat_fun_def` extension is completely separate and does not affect existing code.

## Future Enhancements

1. **Advanced Type Syntax**: Support for more complex Rust type syntax (references, slices, etc.)
2. **Pattern Matching in Parameters**: Allow pattern matching in function parameters
3. **Generic Constraints**: Support for trait bounds and where clauses
4. **Multiple Function Definitions**: Support for multiple function definitions in a single extension call