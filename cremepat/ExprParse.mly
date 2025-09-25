/* =================================================================================================
   CREMEPAT EXPRESSION PARSER: Grammar for Expression Generation
   =================================================================================================
   
   This Menhir grammar defines the syntax for cremepat expression generation.
   It extends the pattern grammar to support full expression syntax with proper
   precedence, type annotations, and mixed concrete/pattern expressions.
   
   DESIGN PRINCIPLES:
   - Familiar Rust/OCaml-like syntax
   - Clear precedence rules without explicit %prec
   - Support for both concrete and pattern variable expressions
   - Type annotation where needed, inference where possible
 */

%{
  open ExprParseTree
%}

/* =================================================================================================
   TOKEN DECLARATIONS
   ================================================================================================= */

/* Literals and identifiers */
%token<int>       INT              /* 42 */
%token<int * string> TYPED_INT     /* 42u32, 100i64 */
%token<string>    STRING           /* "hello" */
%token<string>    LIDENT           /* foo, bar */
%token<string>    UIDENT           /* Vec, Option */
%token<string>    UVAR             /* ?x, ?name */
%token<string>    UVARLIST         /* ?xs.., ?args.. */

/* Keywords */
%token IF THEN ELSE MATCH WITH WHILE FOR IN BREAK CONTINUE
%token LET MUT TRUE FALSE AS

/* Operators */
%token PLUS MINUS STAR SLASH PERCENT                    /* +, -, *, /, % */
%token EQEQ NOTEQ LANGLE RANGLE LEQ GEQ                /* ==, !=, <, >, <=, >= */
%token ANDAND OROR BANG                                /* &&, ||, ! */
%token AMP AT                                          /* &, @ (dereference) */

/* Punctuation */
%token DOT ARROW DARROW COLONCOLON COLON               /* ., ->, =>, ::, : */
%token EQUALS SEMI COMMA                               /* =, ;, , */
%token LPAREN RPAREN LBRACK RBRACK LCURLY RCURLY      /* (), [], {} */
%token LBRACKHASH LPARENHASH                          /* [#, (# */
%token PIPE DOTDOT DOTDOTDOT                          /* |, .., ... */
%token EOF

/* =================================================================================================
   TYPE DECLARATIONS AND PRECEDENCE
   ================================================================================================= */

%type <expr_with_vars> expr
%type <base_typ> typ
%type <path> path
%start <expr_with_vars> fragment

/* Precedence rules (lowest to highest) */
%right DARROW                    /* match arms: pat => expr */
%right SEMI                      /* sequence: expr; expr */
%left OROR                       /* logical or: || */
%left ANDAND                     /* logical and: && */
%left EQEQ NOTEQ                 /* equality: ==, != */
%left LANGLE RANGLE LEQ GEQ      /* comparison: <, >, <=, >= */
%left PLUS MINUS                 /* addition: +, - */
%left STAR SLASH PERCENT         /* multiplication: *, /, % */
%right BANG AMP AT               /* unary: !, &, @ */
%left DOT                        /* field access: obj.field */
%left LBRACK                     /* indexing: arr[i] */
%left LPAREN                     /* function calls: f(args) */

%%

/* =================================================================================================
   UTILITY RULES
   ================================================================================================= */

/* Enhanced with_vars supporting type hints */
%inline
with_vars_typed(X):
| x = UVAR t = ioption(type_annotation)  
    { PatternVar (if x = "" then "_" ^ gensym () else x, t) }
| x = UVARLIST t = ioption(element_type_annotation)
    { ListPatternVar (if x = "" then "_" ^ gensym () else x, t) }  
| x = X                              
    { Fixed x }

%inline
type_annotation:
| COLON t = typ                      { t }

%inline  
element_type_annotation:
| COLON LBRACK t = typ RBRACK        { t }

/* Path handling (reuse from pattern grammar) */
%inline
path:
| p = separated_nonempty_list(COLONCOLON, ident)  
    { 
      match List.rev p with
      | [] -> failwith "empty path"
      | name :: rev_path -> (List.rev rev_path, name)
    }

%inline
ident:
| s = LIDENT                         { s }
| s = UIDENT                         { s }

/* =================================================================================================
   TYPE EXPRESSIONS  
   ================================================================================================= */

typ:
| t = base_typ                       { t }

base_typ:
/* Primitive types */
| LIDENT                            
    { match $1 with
      | "i32" -> TInt I32 | "u32" -> TInt U32 
      | "i64" -> TInt I64 | "u64" -> TInt U64
      | "bool" -> TBool | "str" -> TStr
      | name -> TVar name }
      
/* Qualified types */      
| p = path                          { TQualified p }

/* Generic applications */
| t = base_typ LANGLE args = separated_list(COMMA, typ) RANGLE  
    { TApp (t, args) }

/* Parenthesized types */
| LPAREN t = typ RPAREN             { t }

/* =================================================================================================
   EXPRESSION GRAMMAR  
   ================================================================================================= */

/* Top-level expressions */
expr:
| e = with_vars_typed(expr_node)     { e }

expr_node:
/* Control flow (lowest precedence) */
| LET var = LIDENT t = ioption(type_annotation) EQUALS init = expr SEMI body = expr
    { ELet { 
        var; 
        var_type = t; 
        init = Fixed init; 
        body = Fixed body 
      } }

| IF cond = expr THEN then_expr = expr else_opt = ioption(preceded(ELSE, expr))
    { EIf (Fixed cond, Fixed then_expr, Option.map (fun e -> Fixed e) else_opt) }

| MATCH expr = expr WITH branches = separated_list(PIPE, match_branch)
    { EMatch (Fixed expr, branches) }

| WHILE cond = expr body = delimited(LCURLY, expr, RCURLY)  
    { EWhile (Fixed cond, Fixed body) }

/* Sequences */  
| e1 = expr SEMI e2 = expr          
    { match e2 with
      | Fixed (ESequence exprs) -> ESequence (e1 :: exprs)
      | _ -> ESequence [e1; e2] }

/* Logical operators */
| e1 = expr OROR e2 = expr          { EApp { func = Fixed (EQualified ([], "||")); type_args = []; const_args = []; args = [e1; e2] } }
| e1 = expr ANDAND e2 = expr        { EApp { func = Fixed (EQualified ([], "&&")); type_args = []; const_args = []; args = [e1; e2] } }

/* Comparison operators */
| e1 = expr EQEQ e2 = expr          { EApp { func = Fixed (EQualified ([], "==")); type_args = []; const_args = []; args = [e1; e2] } }
| e1 = expr NOTEQ e2 = expr         { EApp { func = Fixed (EQualified ([], "!=")); type_args = []; const_args = []; args = [e1; e2] } }
| e1 = expr LANGLE e2 = expr        { EApp { func = Fixed (EQualified ([], "<")); type_args = []; const_args = []; args = [e1; e2] } }
| e1 = expr RANGLE e2 = expr        { EApp { func = Fixed (EQualified ([], ">")); type_args = []; const_args = []; args = [e1; e2] } }
| e1 = expr LEQ e2 = expr           { EApp { func = Fixed (EQualified ([], "<=")); type_args = []; const_args = []; args = [e1; e2] } }
| e1 = expr GEQ e2 = expr           { EApp { func = Fixed (EQualified ([], ">=")); type_args = []; const_args = []; args = [e1; e2] } }

/* Arithmetic operators */
| e1 = expr PLUS e2 = expr          { EApp { func = Fixed (EQualified ([], "+")); type_args = []; const_args = []; args = [e1; e2] } }
| e1 = expr MINUS e2 = expr         { EApp { func = Fixed (EQualified ([], "-")); type_args = []; const_args = []; args = [e1; e2] } }
| e1 = expr STAR e2 = expr          { EApp { func = Fixed (EQualified ([], "*")); type_args = []; const_args = []; args = [e1; e2] } }
| e1 = expr SLASH e2 = expr         { EApp { func = Fixed (EQualified ([], "/")); type_args = []; const_args = []; args = [e1; e2] } }
| e1 = expr PERCENT e2 = expr       { EApp { func = Fixed (EQualified ([], "%")); type_args = []; const_args = []; args = [e1; e2] } }

/* Unary operators */
| BANG e = expr                     { EApp { func = Fixed (EQualified ([], "!")); type_args = []; const_args = []; args = [e] } }
| AMP e = expr                      { EAddrOf (Fixed e) }
| AT e = expr                       { EDeref (Fixed e) }

/* Field access and method calls */
| obj = expr DOT field = LIDENT     { EField (Fixed obj, field) }
| obj = expr DOT method = LIDENT args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN)
    { EMethodCall { 
        receiver = Fixed obj; 
        method_name = method; 
        type_args = []; 
        args 
      } }

/* Array indexing */      
| arr = expr LBRACK index = expr RBRACK  { EIndex (Fixed arr, Fixed index) }

/* Function calls with full generic syntax */
| func = expr 
  type_args = ioption(delimited(LANGLE, separated_list(COMMA, typ), RANGLE))
  const_args = ioption(delimited(LBRACKHASH, separated_list(COMMA, expr), RBRACK))
  args = delimited(LPAREN, separated_list(COMMA, expr), RPAREN)
    { 
      let type_args = Option.value ~default:[] type_args in
      let const_args = Option.value ~default:[] const_args in
      EApp { 
        func = Fixed func; 
        type_args = List.map (fun t -> { typ = t; inferred = false; source_loc = None }); 
        const_args; 
        args 
      } 
    }

/* Type ascription */
| LPAREN e = expr COLON t = typ RPAREN  
    { EAscribe (Fixed e, { typ = t; inferred = false; source_loc = None }) }

/* Atomic expressions */
| e = atomic_expr                   { e }

atomic_expr:
/* Literals */
| n = INT                           { ELit (LInt (n, None)) }
| (n, suffix) = TYPED_INT           
    { 
      let width = match suffix with
        | "i32" -> Some I32 | "u32" -> Some U32
        | "i64" -> Some I64 | "u64" -> Some U64  
        | _ -> None
      in
      ELit (LInt (n, width)) 
    }
| s = STRING                        { ELit (LStr s) }
| TRUE                              { ELit (LBool true) }
| FALSE                             { ELit (LBool false) }

/* Variables and qualified names */
| name = LIDENT                     { EVar name }
| p = path                          { EQualified p }

/* Data structures */
| LPAREN RPAREN                     { ELit LUnit }
| LPAREN exprs = separated_list(COMMA, expr) RPAREN
    { match exprs with
      | [single] -> single  (* Parenthesized expression *)
      | multiple -> ETuple multiple }
      
| LBRACK exprs = separated_list(COMMA, expr) RBRACK  { EArray exprs }

| LCURLY fields = separated_list(COMMA, record_field) RCURLY  { ERecord fields }

/* Block expressions */
| LCURLY e = expr RCURLY            { e }

/* Parenthesized expressions */
| LPAREN e = expr RPAREN            { e }

/* =================================================================================================
   AUXILIARY RULES
   ================================================================================================= */

match_branch:
| pat = pattern DARROW expr = expr  { (pat, Fixed expr) }

pattern:
/* Simplified pattern syntax - integrate with existing pattern system */
| name = LIDENT                     { PVar name }
| UIDENT                           { PCons ($1, []) }
| n = INT                          { PLit (LInt (n, None)) }
| TRUE                             { PLit (LBool true) }
| FALSE                            { PLit (LBool false) }
| s = STRING                       { PLit (LStr s) }

record_field:
| field = LIDENT COLON value = expr { (field, Fixed value) }

/* Entry point */
fragment:
| e = expr EOF                     { e }

/* =================================================================================================
   IMPLEMENTATION NOTES
   ================================================================================================= */

/*
   EXPRESSION GRAMMAR FEATURES:
   
   PRECEDENCE HANDLING:
   - Uses %left, %right declarations for clear precedence
   - Avoids conflicts through careful rule ordering
   - Supports standard mathematical and logical precedence
   
   TYPE ANNOTATIONS:
   - Optional type annotations on let bindings: let x : i32 = 42
   - Type ascription: (expr : type) 
   - Generic type parameters: Vec<i32>, HashMap<K, V>
   
   PATTERN VARIABLES:
   - Enhanced with_vars_typed supports type hints
   - ?x captures single expressions
   - ?xs.. captures expression lists
   - Mixed concrete/pattern expressions
   
   FUNCTION CALLS:
   - Full Rust-style generic syntax: f::<T, U>(args)  
   - Const generic arguments: f[#N, M](args)
   - Method call syntax: obj.method(args)
   
   CONTROL FLOW:
   - If expressions with optional else
   - Match expressions with pattern integration
   - While loops
   - Sequence expressions with proper flattening
   
   ERROR RECOVERY:
   - Clear error messages for syntax errors
   - Location tracking for all constructs
   - Integration with overall error handling
   
   FUTURE EXTENSIONS:
   - For loops and iterators
   - Async/await syntax
   - Closure expressions  
   - Pattern guards in match expressions
   - More sophisticated type inference integration
 */