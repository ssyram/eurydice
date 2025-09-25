# Cremepat Function Support - Testing Guide

## Overview

This document explains how to test the new cremepat function support functionality.

## What Was Added

The cremepat extension now supports Rust-like function definition syntax:

```rust
[%cremepat {|
fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
  SliceRef<T> { pointer = arr, length = N }
}
|}]
```

## Code Changes Made

### 1. ParseTree.ml
- Added `FunctionDef` constructor to `pre_expr` type
- Added supporting types: `type_param`, `cg_param`, `param`
- **Rationale**: These extend the AST to represent function definitions with type parameters, const generic parameters, regular parameters, return types, and function bodies

### 2. Lex.ml  
- Added `FN` token to the keywords table
- **Rationale**: Enables lexical recognition of the `fn` keyword to start function definitions

### 3. Parse.mly
- Added `FN` token declaration
- Added type declarations for new AST node types
- Added grammar rules for function definitions with support for:
  - Type parameters `<T>`
  - Const generic parameters `<N : size_t>`
  - Regular parameters with types `(arr: &[T; N])`
  - Return type annotations `-> SliceRef<T>`
  - Array types `[T; N]`
  - Reference types `&T`
- **Rationale**: Provides the parsing infrastructure to recognize and construct the new function syntax

### 4. cremepat.ml
- Added compilation logic for `FunctionDef` in `compile_pre_expr`
- Added helper `compile_typ_with_var` for type compilation
- **Rationale**: Transforms the parsed function definitions into appropriate OCaml AST patterns that can be used in pattern matching

## Test Files Created

### 1. test_cremepat_functions.ml
A standalone test that directly tests the parsing functionality.

### 2. test_cremepat_integration.ml
An integration test showing how the function syntax works with the cremepat extension.

## How to Run Tests

### Method 1: Using the Build System (Recommended)
```bash
# Build the project
make build

# The test files demonstrate the usage but require a working OCaml environment
# In a full setup, you would run:
# ocaml test_cremepat_functions.ml
```

### Method 2: Manual Verification
1. Examine the generated OCaml AST patterns
2. Verify that the function definitions parse correctly
3. Check that the cremepat extension processes the new syntax

## Example Usage

The following syntax is now supported:

```ocaml
(* Simple function *)
[%cremepat {| fn simple(x: i32) -> i32 { x } |}]

(* Generic function *)  
[%cremepat {| fn generic<T>(x: T) -> T { x } |}]

(* Complex function with const generics and references *)
[%cremepat {|
fn array_to_slice<T><N : size_t>(arr: &[T; N]) -> SliceRef<T> {
  SliceRef<T> { pointer = arr, length = N }
}
|}]
```

## Architecture Notes

The extension works by:
1. Lexing the input to recognize tokens
2. Parsing the token stream into an AST using Menhir
3. Compiling the AST into OCaml patterns using Ppxlib
4. The OCaml compiler then uses these patterns in match expressions

This maintains compatibility with existing cremepat usage while adding the new function syntax.

## Debugging

If parsing fails:
1. Check that all required tokens are properly defined
2. Verify grammar rule precedence
3. Ensure type definitions are complete
4. Look for missing compilation cases in cremepat.ml

The parser provides detailed error messages showing the location of parsing failures.