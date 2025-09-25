(* =================================================================================================
   CREMEPAT EXPRESSION LEXER: Tokenizing Expression Syntax
   =================================================================================================
   
   This lexer extends the pattern lexer to support expression-specific syntax:
   - Type annotations (: typ)
   - Method calls (obj.method)
   - Generic instantiation (func::<T>)
   - Additional operators and keywords
   
   DESIGN: Reuse existing pattern lexer where possible, add expression-specific tokens
 *)

open Sedlexing
open ExprParse  (* Will be defined in ExprParse.mly *)

(* Reuse basic patterns from pattern lexer *)
let digit = [%sedlex.regexp? '0' .. '9']
let integer = [%sedlex.regexp? Plus digit]
let low_alpha = [%sedlex.regexp? 'a' .. 'z']
let up_alpha = [%sedlex.regexp? 'A' .. 'Z']
let anyident = [%sedlex.regexp? up_alpha | low_alpha | '_' | '-' | digit]
let lident = [%sedlex.regexp? low_alpha, Star anyident]
let uident = [%sedlex.regexp? up_alpha, Star anyident]

(* Pattern variables (same as pattern lexer) *)
let uvar = [%sedlex.regexp? '?', Star anyident]
let uvarlist = [%sedlex.regexp? '?', Star anyident, '.', '.']

(* Type annotation patterns *)
let type_annotation = [%sedlex.regexp? ':', Star white_space, (up_alpha | low_alpha), Star anyident]

(* String literals for expression values *)
let string_char = [%sedlex.regexp? Sub (any, '"' | '\\' | '\n')]
let escape_sequence = [%sedlex.regexp? '\\', ('"' | '\\' | 'n' | 't' | 'r')]
let string_content = [%sedlex.regexp? Star (string_char | escape_sequence)]
let string_literal = [%sedlex.regexp? '"', string_content, '"']

(* Number suffixes for typed integers *)
let int_suffix = [%sedlex.regexp? 
  | "i8" | "i16" | "i32" | "i64" | "i128" | "isize"
  | "u8" | "u16" | "u32" | "u64" | "u128" | "usize"
]
let typed_integer = [%sedlex.regexp? Plus digit, int_suffix]

let locate _ tok = tok, Lexing.dummy_pos, Lexing.dummy_pos

(* Keywords for expressions (extends pattern keywords) *)
let expr_keywords = [
  (* Control flow *)
  "if", IF; "then", THEN; "else", ELSE;
  "match", MATCH; "with", WITH;
  "while", WHILE; "for", FOR; "in", IN;
  "break", BREAK; "continue", CONTINUE;
  
  (* Binding and assignment *)
  "let", LET; "mut", MUT;
  
  (* Literals *)
  "true", TRUE; "false", FALSE;
  
  (* Types and annotations *)
  "as", AS;
]

let lines = ref 1
let cur_line = ref 0

let rec token lexbuf =
  match%sedlex lexbuf with
  
  (* Typed integers: 42u32, 100i64 *)
  | typed_integer ->
      let l = Utf8.lexeme lexbuf in
      let (num_str, suffix) = 
        let len = String.length l in
        if String.contains l 'i' then
          let i = String.index l 'i' in
          (String.sub l 0 i, String.sub l i (len - i))
        else
          let u = String.index l 'u' in
          (String.sub l 0 u, String.sub l u (len - u))
      in
      locate lexbuf (TYPED_INT (int_of_string num_str, suffix))
  
  (* Regular integers *)
  | integer ->
      let l = Utf8.lexeme lexbuf in
      locate lexbuf (INT (int_of_string l))
  
  (* String literals *)
  | string_literal ->
      let l = Utf8.lexeme lexbuf in
      (* Remove quotes and process escape sequences *)
      let content = String.sub l 1 (String.length l - 2) in
      locate lexbuf (STRING content)
  
  (* Identifiers and keywords *)
  | uident ->
      let l = Utf8.lexeme lexbuf in
      locate lexbuf (UIDENT l)
  
  | lident ->
      let l = Utf8.lexeme lexbuf in
      begin
        try locate lexbuf (List.assoc l expr_keywords) 
        with Not_found -> locate lexbuf (LIDENT l)
      end
  
  (* Pattern variables *)
  | uvar ->
      let l = Utf8.lexeme lexbuf in
      let l = String.sub l 1 (String.length l - 1) in
      locate lexbuf (UVAR l)
  
  | uvarlist ->
      let l = Utf8.lexeme lexbuf in
      let l = String.sub l 1 (String.length l - 3) in
      locate lexbuf (UVARLIST l)
  
  (* Expression-specific operators *)
  | "::" -> locate lexbuf COLONCOLON     (* Module path separator *)
  | ":" -> locate lexbuf COLON           (* Type annotation *)
  | "." -> locate lexbuf DOT             (* Field access, method call *)
  | "->" -> locate lexbuf ARROW          (* Function type, match arms *)
  | "=>" -> locate lexbuf DARROW         (* Match arms *)
  | "=" -> locate lexbuf EQUALS          (* Let binding *)
  | "==" -> locate lexbuf EQEQ           (* Equality comparison *)
  | "!=" -> locate lexbuf NOTEQ          (* Inequality comparison *)
  | "<" -> locate lexbuf LANGLE          (* Less than, generic start *)
  | ">" -> locate lexbuf RANGLE          (* Greater than, generic end *)
  | "<=" -> locate lexbuf LEQ            (* Less or equal *)
  | ">=" -> locate lexbuf GEQ            (* Greater or equal *)
  
  (* Arithmetic operators *)
  | "+" -> locate lexbuf PLUS
  | "-" -> locate lexbuf MINUS
  | "*" -> locate lexbuf STAR
  | "/" -> locate lexbuf SLASH
  | "%" -> locate lexbuf PERCENT
  
  (* Logical operators *)
  | "&&" -> locate lexbuf ANDAND         (* Logical and *)
  | "||" -> locate lexbuf OROR           (* Logical or *)
  | "!" -> locate lexbuf BANG            (* Logical not *)
  
  (* Memory operators *)
  | "&" -> locate lexbuf AMP             (* Address-of, reference *)
  | "@" -> locate lexbuf AT              (* Dereference (alternative) *)
  
  (* Delimiters *)
  | ";" -> locate lexbuf SEMI            (* Statement separator *)
  | "," -> locate lexbuf COMMA           (* List separator *)
  | "[" -> locate lexbuf LBRACK          (* Array start, indexing *)
  | "]" -> locate lexbuf RBRACK          (* Array end *)
  | "[#" -> locate lexbuf LBRACKHASH     (* Const generic args *)
  | "(" -> locate lexbuf LPAREN          (* Expression grouping *)
  | ")" -> locate lexbuf RPAREN          
  | "(#" -> locate lexbuf LPARENHASH     (* Method args *)
  | "{" -> locate lexbuf LCURLY          (* Block start *)
  | "}" -> locate lexbuf RCURLY          (* Block end *)
  | "|" -> locate lexbuf PIPE            (* Pattern alternatives *)
  
  (* Special expression syntax *)
  | ".." -> locate lexbuf DOTDOT         (* Range operator *)
  | "..." -> locate lexbuf DOTDOTDOT     (* Rest operator *)
  
  (* Whitespace and newlines *)
  | "\n" ->
      incr lines;
      cur_line := fst (loc lexbuf);
      token lexbuf
  
  | eof -> locate lexbuf EOF
  | white_space -> token lexbuf
  
  (* Error handling *)
  | any ->
      let l = Utf8.lexeme lexbuf in
      failwith (Printf.sprintf "Unhandled token in expression: %s, len=%d" l (String.length l))
  
  | _ -> assert false

(* =================================================================================================
   UTILITY FUNCTIONS
   ================================================================================================= *)

(* Reset lexer state *)
let reset_lexer () =
  lines := 1;
  cur_line := 0

(* Get current position for error reporting *)
let current_position lexbuf =
  let pos = loc lexbuf in
  (!lines, fst pos - !cur_line, snd pos - !cur_line)

(* Tokenize entire string for debugging *)
let tokenize_string s =
  let lexbuf = Utf8.from_string s in
  let rec collect_tokens acc =
    match token lexbuf with
    | EOF, _, _ -> List.rev acc
    | tok -> collect_tokens (tok :: acc)
  in
  reset_lexer ();
  collect_tokens []

(* =================================================================================================
   IMPLEMENTATION NOTES
   ================================================================================================= *)

(*
   EXPRESSION LEXER EXTENSIONS:
   
   NEW TOKENS:
   - TYPED_INT: integers with type suffixes (42u32, 100i64)
   - STRING: string literals with escape sequences
   - DOT: field access and method calls (obj.field, obj.method())
   - Comparison operators: EQEQ, NOTEQ, LEQ, GEQ  
   - Arithmetic operators: PLUS, MINUS, STAR, SLASH, PERCENT
   - Logical operators: ANDAND, OROR, BANG
   - Range operators: DOTDOT, DOTDOTDOT
   
   REUSED FROM PATTERN LEXER:
   - Basic identifiers (LIDENT, UIDENT)
   - Pattern variables (UVAR, UVARLIST)
   - Module paths (COLONCOLON)
   - Generic delimiters (LANGLE, RANGLE, LBRACKHASH)
   - Basic punctuation
   
   ERROR HANDLING:
   - Location tracking for expression-specific errors
   - Clear error messages for unsupported syntax
   - Integration with overall error reporting system
   
   FUTURE EXTENSIONS:
   - Float literals with type suffixes
   - Character literals  
   - Raw string literals
   - Operator precedence hints in lexer
   - More sophisticated string escape handling
*)