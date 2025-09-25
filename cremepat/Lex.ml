(* =================================================================================================
   CREMEPAT LEXICAL ANALYZER: Tokenizing the Pattern Matching Language
   =================================================================================================
   
   This module implements the lexical analysis phase of cremepat compilation.
   It converts raw text input into a stream of tokens that the parser can consume.
   
   LEARNING GUIDE: Understanding Lexical Analysis
   ---------------------------------------------
   1. Regular expressions define token patterns using sedlex syntax
   2. The main 'token' function matches input against patterns sequentially  
   3. Keywords are handled via a lookup table to distinguish from identifiers
   4. Location tracking enables error reporting with precise source positions
   5. Unification variables (?name, ?name..) are lexically distinct from identifiers
   
   KEY INNOVATION: Unification Variable Tokens
   ------------------------------------------
   Cremepat introduces special tokens for pattern matching:
   - UVAR: ?name (single element pattern variables)
   - UVARLIST: ?name.. (list pattern variables) 
   
   This allows the parser to distinguish between literal identifiers and pattern variables
   at the syntactic level, enabling flexible pattern matching syntax.
 *)

open Sedlexing
open Parse

(* =================================================================================================
   TOKEN PATTERN DEFINITIONS
   =================================================================================================
   
   These regular expressions define the lexical structure of cremepat syntax.
   Each pattern matches a specific category of input and produces corresponding tokens.
 *)

(* Basic character classes *)
let digit = [%sedlex.regexp? '0' .. '9']
let integer = [%sedlex.regexp? Plus digit]           (* One or more digits *)
let low_alpha = [%sedlex.regexp? 'a' .. 'z'] 
let up_alpha = [%sedlex.regexp? 'A' .. 'Z']
let anyident = [%sedlex.regexp? up_alpha | low_alpha | '_' | '-' | digit]

(* Identifier patterns - crucial for distinguishing variables, constructors, and keywords *)
let lident = [%sedlex.regexp? low_alpha, Star anyident]  (* lowercase identifiers: foo, bar_baz *)
let uident = [%sedlex.regexp? up_alpha, Star anyident]   (* uppercase identifiers: Foo, Bar_Baz *)

(* Unification variable patterns - the key innovation of cremepat *)
let uvar = [%sedlex.regexp? '?', Star anyident]          (* ?name - single pattern variable *)
let uvarlist = [%sedlex.regexp? '?', Star anyident, '.', '.']  (* ?name.. - list pattern variable *)

(* Dummy location for tokens (real location tracking handled by sedlex) *)
let locate _ tok = tok, Lexing.dummy_pos, Lexing.dummy_pos

(* Keyword lookup table: Maps reserved words to their token types.
   This ensures that 'match', 'while', etc. are parsed as control structures
   rather than regular identifiers.
 *)
let keywords =
  [ 
    "match", MATCH;   (* pattern matching construct *)
    "true", TRUE;     (* boolean literal *)
    "break", BREAK;   (* loop break statement *)
    "false", FALSE;   (* boolean literal *)
    "while", WHILE;   (* loop construct *)
    "let", LET        (* variable binding *)
  ]

(* =================================================================================================
   LOCATION TRACKING FOR ERROR REPORTING
   =================================================================================================
   
   These variables maintain source position information to provide meaningful
   error messages when parsing fails.
 *)
let lines = ref 1        (* Current line number *)
let cur_line = ref 0     (* Character offset of current line start *)

(* =================================================================================================  
   MAIN TOKENIZATION FUNCTION
   =================================================================================================
   
   This is the core of the lexical analyzer. It uses pattern matching on the input stream
   to identify tokens and produce the corresponding token types for the parser.
   
   ALGORITHM:
   1. Try to match input against each pattern in priority order
   2. For identifiers, check keyword table first  
   3. Extract lexeme text and convert to appropriate token
   4. Handle whitespace and newlines transparently
   5. Report errors for unrecognized input
 *)
let rec token lexbuf =
  match%sedlex lexbuf with
  
  (* Numeric literals *)
  | integer ->
      let l = Utf8.lexeme lexbuf in
      locate lexbuf (INT (int_of_string l))
      
  (* Uppercase identifiers (constructors, modules) *)    
  | uident ->
      let l = Utf8.lexeme lexbuf in
      locate lexbuf (UIDENT l)
      
  (* Lowercase identifiers with keyword checking *)
  | lident ->
      let l = Utf8.lexeme lexbuf in
      begin
        try 
          (* Check if this is a reserved keyword *)
          locate lexbuf (List.assoc l keywords) 
        with Not_found -> 
          (* Regular lowercase identifier *)
          locate lexbuf (LIDENT l)
      end
      
  (* UNIFICATION VARIABLES - The core of cremepat's pattern matching power *)
  | uvar ->
      let l = Utf8.lexeme lexbuf in
      (* Strip the leading '?' character *)
      let l = String.sub l 1 (String.length l - 1) in
      locate lexbuf (UVAR l)
      
  | uvarlist ->
      let l = Utf8.lexeme lexbuf in  
      (* Strip the leading '?' and trailing '..' *)
      let l = String.sub l 1 (String.length l - 3) in
      locate lexbuf (UVARLIST l)
      
  (* Punctuation and operators - must match the grammar expectations *)
  | "&" -> locate lexbuf AMP                    (* Address-of operator *)
  | ";" -> locate lexbuf SEMI                   (* Statement separator *)
  | "->" -> locate lexbuf ARROW                 (* Match arm separator *)
  | "," -> locate lexbuf COMMA                  (* List separator *)
  | "=" -> locate lexbuf EQUALS                 (* Assignment/binding *)
  | "[#" -> locate lexbuf LBRACKHASH           (* Const generic args start *)
  | "[" -> locate lexbuf LBRACK                (* Array/index start *)
  | "]" -> locate lexbuf RBRACK                (* Array/index end *)
  | "<" -> locate lexbuf LANGLE                (* Type args start *)
  | ">" -> locate lexbuf RANGLE                (* Type args end *)
  | "{" -> locate lexbuf LCURLY                (* Block/record start *)  
  | "}" -> locate lexbuf RCURLY                (* Block/record end *)
  | "(#" -> locate lexbuf LPARENHASH           (* Method args start *)
  | "(" -> locate lexbuf LPAREN                (* Function args start *)
  | ")" -> locate lexbuf RPAREN                (* Function args end *)
  | "::" -> locate lexbuf COLONCOLON           (* Path separator *)
  | ":" -> locate lexbuf COLON                 (* Type annotation *)
  
  (* Whitespace and line tracking *)
  | "\n" ->
      (* Update line tracking for error reporting *)
      incr lines;
      cur_line := fst (loc lexbuf);
      token lexbuf  (* Continue tokenizing *)
      
  | eof -> locate lexbuf EOF                    (* End of input *)
  | white_space -> token lexbuf                 (* Skip whitespace *)
  
  (* Error handling for unrecognized input *)
  | any ->
      let l = Utf8.lexeme lexbuf in
      failwith (Printf.sprintf "unhandled token: %s, len=%d" l (String.length l))
      
  | _ -> assert false  (* Should never reach here with well-formed sedlex patterns *)
