(* Strictly a parse tree *)
type pre_function_def = {
  name: string;
  type_params: typ list;
  const_params: (string * typ) list; (* For `N : size_t` syntax *)
  params: (string * typ) list;
  return_type: typ;
  body: expr;
}

and function_def = pre_function_def with_vars

and pre_expr =
  (* Binding most loosely *)
  | Let of string * expr * expr
  | Sequence of expr list
  | App of { head : expr; cgs : expr list; methods : expr list; ts : typ list; args : expr list }
  | Addr of expr
  | Index of expr * expr
  (* Atomic -- we terminate matches and loops using braces, we are not barbarians. *)
  | While of expr * expr
  | Match of expr * branch list
  | Record of (string * expr) list
  | Int of int
  | Qualified of path
  | BoundVar of string
  | Break
  | Bool of bool
  | FunctionDef of function_def

and expr = pre_expr with_vars
and 'a with_vars = PatternVar of string | ListPatternVar of string | Fixed of 'a
and path = path_item list
and path_item = Name of string | Wild | Var of string
and branch = pat * expr
and pre_pat = Cons of string * pat list
and pat = pre_pat with_vars
and pre_typ = TQualified of path | TApp of typ * typ list
and typ = pre_typ with_vars

let gensym =
  let r = ref 0 in
  fun () ->
    incr r;
    "$x" ^ string_of_int !r
