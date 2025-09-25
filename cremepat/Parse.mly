(* =================================================================================================
   CREMEPAT PARSER: Grammar Definition for Pattern Matching Language  
   =================================================================================================
   
   This Menhir grammar defines the syntax and precedence rules for cremepat's pattern 
   matching language. It transforms tokens from the lexer into the ParseTree AST.
   
   LEARNING GUIDE: Understanding Parser Architecture
   -----------------------------------------------
   1. Token declarations define the lexical interface with the lexer
   2. Type annotations specify which AST nodes each rule produces  
   3. Precedence is controlled by rule ordering and explicit %prec directives
   4. Actions in {} build AST nodes using ParseTree constructors
   5. Parameterized rules (like with_vars) enable code reuse
   
   KEY INNOVATION: Unification Variables in Grammar
   -----------------------------------------------
   The grammar seamlessly integrates pattern variables (?name) and list variables (?name..)
   at the syntactic level, allowing them to appear anywhere in the expression structure.
   This is implemented through the 'with_vars' parameterized rule.
   
   EXPRESSION PRECEDENCE HIERARCHY (lowest to highest):
   - expr: let bindings, sequences  
   - seq_expr: semicolon sequences
   - app_expr: function application with generics
   - index_expr: array/buffer indexing
   - atomic_expr: literals, parenthesized expressions, control structures
 *)

%{
  open ParseTree
%}

(* =================================================================================================
   TOKEN DECLARATIONS
   =================================================================================================
   
   These declarations define the interface between the lexer and parser.
   Each token type corresponds to specific lexical patterns from Lex.ml
 *)

%token<int>     INT         (* Integer literals: 42, 1000 *)
%token<string>  UIDENT      (* Uppercase identifiers: Vec, Option *)
%token<string>  LIDENT      (* Lowercase identifiers: foo, bar *)
%token<string>  UVAR        (* Pattern variables: ?x, ?name *)
%token<string>  UVARLIST    (* List pattern variables: ?xs.., ?args.. *)

(* Punctuation and operators *)
%token          EOF COMMA EQUALS LBRACK RBRACK LBRACKHASH LANGLE RANGLE LCURLY RCURLY
%token          COLON COLONCOLON AMP LPAREN RPAREN LPARENHASH SEMI

(* Keywords *)
%token          MATCH TRUE FALSE LET WHILE BREAK ARROW

(* =================================================================================================
   TYPE DECLARATIONS & ENTRY POINT
   =================================================================================================
   
   These declarations specify what AST types the parser rules produce
 *)
%type <expr> expr           (* Main expression type *)
%type <path_item> path_item (* Components of qualified paths *)
%type <pat> pat            (* Pattern matching constructs *)
%type <typ> typ            (* Type expressions *)
%start <expr> fragment     (* Parser entry point *)

%%

(* =================================================================================================
   IDENTIFIER RULES
   =================================================================================================
   
   Basic identifier handling with inline optimization for efficiency
 *)

%inline
uident:
| u = UIDENT    { u }

%inline  
lident:
| l = LIDENT    { l }

%inline
ident:
| s = LIDENT    { s }
| s = UIDENT    { s }

(* =================================================================================================
   PATH SYSTEM: Qualified Names and Module Paths
   =================================================================================================
   
   Handles qualified identifiers like std::vec::Vec or crate::module::function.
   Supports pattern variables in path components for flexible matching.
 *)

path_item:
| i = ident     { Name i }                                    (* Concrete identifier *)
| p = UVAR      { if p = "" then Wild else Var p }          (* Pattern variable *)
| _p = UVARLIST { failwith "TODO" }                         (* List variables not yet supported in paths *)

%inline
path:
| p = iseparated_twoplus_list(COLONCOLON, path_item)  { p }  (* At least two components separated by :: *)

(* =================================================================================================
   PARAMETERIZED HELPER RULES
   =================================================================================================
   
   These utility rules enable code reuse and implement core cremepat functionality
 *)

(* Helper for lists with at least 2 elements separated by a delimiter *)
%inline iseparated_twoplus_list(separator, X):
  x1 = X; separator; x2 = X                                         { [ x1; x2 ] }
| x1 = X; separator; x2 = X; separator; xs = separated_nonempty_list(separator, X)  { x1 :: x2 :: xs }

(* THE CORE INNOVATION: with_vars parameterized rule
   
   This rule enables any AST node to be wrapped with unification variables.
   It's the foundation of cremepat's pattern matching flexibility.
   
   Usage: with_vars(some_rule) allows ?var or ?var.. at any position where some_rule appears
 *)
%inline
with_vars(X):
| x = UVAR      { PatternVar (if x = "" then "_" ^ gensym () else x) }    (* Single pattern variable *)
| x = UVARLIST  { ListPatternVar (if x = "" then "_" ^ gensym () else x) } (* List pattern variable *)  
| x = X         { Fixed x }                                                (* Concrete value *)

(* Helper for concrete (non-variable) values only *)
%inline
fixed(X):
| x = X         { Fixed x }

(* =================================================================================================
   TYPE SYSTEM GRAMMAR
   =================================================================================================
   
   Defines the syntax for type expressions, supporting both qualified types
   and generic type applications.
 *)

pre_typ:
| t = typ ts = delimited(LANGLE, separated_list(COMMA, typ), RANGLE)  { TApp (t, ts) }  (* Generic instantiation: Vec<i32> *)
| p = path                                                            { TQualified p }   (* Qualified type name: std::vec::Vec *)

typ:
| t = with_vars(pre_typ)    { t }   (* Types with pattern variables: ?T, Vec<?T> *)

(* =================================================================================================
   PATTERN MATCHING GRAMMAR  
   =================================================================================================
   
   Handles pattern syntax for match expressions. Currently supports constructor patterns.
 *)

pre_pat:
| u = uident                                                          { Cons (u, []) }      (* Unit constructor: Some *)
| u = uident p = delimited(LPAREN, separated_list(COMMA, pat), RPAREN) { Cons (u, p) }       (* Constructor with args: Some(42) *)  
| u = uident p = pat                                                  { Cons (u, [ p ]) }   (* Constructor with single arg: Some ?x *)

pat:
| t = with_vars(pre_pat)    { t }   (* Patterns with unification variables *)

(* =================================================================================================
   EXPRESSION GRAMMAR: The Heart of Cremepat
   =================================================================================================
   
   This section defines the complete expression syntax with proper precedence.
   The grammar is carefully structured to handle operator precedence without
   explicit precedence declarations, using rule ordering instead.
   
   PRECEDENCE LEVELS (from lowest to highest):
   1. expr: let bindings (lowest precedence - binds loosely)
   2. seq_expr: semicolon sequences  
   3. app_expr: function calls with generic parameters
   4. index_expr: array indexing operations
   5. atomic_expr: literals and parenthesized expressions (highest precedence)
 *)

(* TOP LEVEL: Let bindings and sequences *)
expr:
| e = fixed(pre_expr)   { e }   (* Concrete let expressions *)
| e = seq_expr         { e }   (* Defer to sequence level *)

pre_expr:
| LET b = lident EQUALS e1 = app_expr SEMI e2 = expr    { Let (b, e1, e2) }   (* let x = expr; body *)

(* SEQUENCE LEVEL: Semicolon-separated expressions *)  
seq_expr:
| e = fixed(pre_seq_expr)  { e }   (* Concrete sequences *)
| e = app_expr            { e }   (* Defer to application level *)

pre_seq_expr:
| e1 = app_expr SEMI e2 = seq_expr  { 
    (* Flatten sequences for cleaner AST: e1; (e2; e3) becomes Sequence [e1; e2; e3] *)
    match e2 with 
    | Fixed (Sequence e2) -> Sequence (e1 :: e2) 
    | _ -> Sequence [ e1; e2 ] 
  }

(* APPLICATION LEVEL: Function calls with full Rust-style generic syntax *)
app_expr:
| e = fixed(pre_app_expr) { e }    (* Concrete function calls *)
| e = index_expr         { e }    (* Defer to indexing level *)

pre_app_expr:
(* Complex function call syntax: f[#cg1, cg2](#m1, m2)<T1, T2>(arg1, arg2)
   - [#...] : const generic arguments
   - (#...) : method arguments  
   - <...>  : type arguments
   - (...) : regular arguments
 *)
| head = app_expr
  cgs = ioption(delimited(LBRACKHASH, separated_list(COMMA, expr), RBRACK))
  methods = ioption(delimited(LPARENHASH, separated_list(COMMA, expr), RPAREN))  
  ts = ioption(delimited(LANGLE, separated_list(COMMA, typ), RANGLE))
  args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN)
  {
    let cgs = Option.value ~default:[] cgs in
    let methods = Option.value ~default:[] methods in  
    let ts = Option.value ~default:[] ts in
    App { head; cgs; methods; ts; args }
  }
| AMP e = index_expr    { Addr e }    (* Address-of operator: &expr *)

(* INDEXING LEVEL: Array and buffer access *)
index_expr:
| e = fixed(pre_index_expr) { e }     (* Concrete indexing *)
| e = atomic_expr          { e }     (* Defer to atomic level *)

pre_index_expr:
| e1 = index_expr e2 = delimited(LBRACK, expr, RBRACK)  { Index (e1, e2) }  (* arr[index] *)

(* ATOMIC LEVEL: Literals, variables, and parenthesized expressions *)
atomic_expr:
| e = with_vars(pre_atomic_expr)           { e }   (* Atomic expressions with pattern variables *)
| e = delimited(LPAREN, expr, RPAREN)     { e }   (* Parenthesized expressions *)

pre_atomic_expr:
(* Control structures *)
| WHILE e1 = index_expr e2 = delimited(LCURLY, expr, RCURLY)  { While (e1, e2) }  (* while cond { body } *)
| MATCH e = index_expr bs = delimited(LCURLY, separated_list(COMMA, separated_pair(pat, ARROW, expr)), RCURLY) 
    { Match (e, bs) }  (* match expr { pat => expr, ... } *)

(* Data structures *)    
| e = delimited(LCURLY, separated_nonempty_list(COMMA, separated_pair(lident, COLON, expr)), RCURLY)  
    { Record e }       (* { field: expr, ... } *)

(* Literals and variables *)
| i = INT              { Int i }        (* Integer literals *)
| p = path             { Qualified p }  (* Qualified names: std::vec::Vec *)  
| x = lident           { BoundVar x }   (* Local variables *)
| BREAK                { Break }        (* break statement *)
| FALSE                { Bool false }   (* Boolean literals *)
| TRUE                 { Bool true }

(* =================================================================================================
   ENTRY POINT
   ================================================================================================= *)

fragment:
| e = expr EOF    { e }   (* Parse a complete expression followed by end-of-file *)

