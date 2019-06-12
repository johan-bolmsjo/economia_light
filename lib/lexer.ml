type token_datum = {
  value    :string;
  location :Location.t;
}

type token =
  | Comment of token_datum
  | String of token_datum
  | Identifier of token_datum
  | Number of token_datum
  | Colon of token_datum
  | Equal of token_datum
  | LBracket of token_datum
  | RBracket of token_datum
  | Space of token_datum
  | Newline of token_datum

let datum_of_token = function
  | Comment d | String d | Identifier d | Number d | Colon d
  | Equal d | LBracket d | RBracket d | Space d | Newline d
    -> d

let string_of_token token =
  let kind = match token with
    | Comment _    -> "Comment"
    | String _     -> "String"
    | Identifier _ -> "Identifier"
    | Number _     -> "Number"
    | Colon _      -> "Colon"
    | Equal _      -> "Equal"
    | LBracket _   -> "LBracket"
    | RBracket _   -> "RBracket"
    | Space _      -> "Space"
    | Newline _    -> "Newline"
  in
  let datum = datum_of_token token in
  Printf.sprintf "%s: \"%s\" %s" kind (String.escaped datum.value) (Location.string_of datum.location)

type context = {
  stream                   :char Stream.t;
  buf                      :Buffer.t;
  mutable location_token   :Location.t;  (* Location at token reset *)
  mutable location_current :Location.t;  (* Continuously updated location *)
}

let context_of_char_stream stream =
  { stream
  ; buf              = Buffer.create 100
  ; location_token   = Location.zero
  ; location_current = Location.zero
  }

(* Reset token lexing state. *)
let reset_lexing_state ctx =
  Buffer.reset ctx.buf;
  ctx.location_token <- ctx.location_current

(* Get lexed characters as token of type [typ] and reset token lexing state. *)
let get_token_datum ctx =
  let datum = { value = Buffer.contents ctx.buf; location = ctx.location_token } in
  reset_lexing_state ctx; datum

(* Special get_token_datum function to strip string quotes out of the lexed data for strings. *)
let get_string_token_datum ctx =
  let len = Buffer.length ctx.buf in
  let location = { ctx.location_token with column = ctx.location_token.column + 1 } in
  let datum = { value = Buffer.sub ctx.buf 1 (len - 2); location } in
  reset_lexing_state ctx; datum

let peek_char ctx = Stream.peek ctx.stream

(* Save previously peeked character.

   Line and column information is updated based on character value.

   The character is added to the temporary lexer context buffer for possible
   later inclusion in a lexed token.
*)
let save_char ctx c =
  Buffer.add_char ctx.buf c;
  Stream.junk ctx.stream;
  let location = ctx.location_current in
  match c with
  | '\n' -> ctx.location_current <- { line = location.line + 1; column = 0 }
  | '\r' -> () (* Invisitble control char *)
  | '\t' -> ctx.location_current <- { location with column = location.column + 8 } (* Tabs are messy, count something. *)
  | _    -> ctx.location_current <- { location with column = location.column + 1 }

(*
 ** Token lexing helper functions.
 *)

(* Lex single character token. *)
let lex_single_char ctx c =
  save_char ctx c;
  get_token_datum ctx

(* Lex characters until predicate function says no. *)
let lex_until ctx pred ?(keep_last=false) ?(get_token_datum=get_token_datum) c =
  save_char ctx c;
  let rec consume () =
    match peek_char ctx with
    | Some c ->
      if (pred c) then
        begin
          save_char ctx c;
          consume ()
        end
      else
        begin
          (if keep_last then save_char ctx c);
          get_token_datum ctx
        end
    | None -> raise (Error.Error(Lexer_error(Unexpected_eof, ctx.location_token)))
  in
  consume ()

(* Lex characters until predicate function says no or EOF occurs. *)
let lex_until_or_eof ctx pred ?(keep_last=false) c =
  try
    lex_until ctx pred ~keep_last c
  with
    Error.Error(Lexer_error(Unexpected_eof, _)) -> get_token_datum ctx

(*
 ** Token type lexing functions.
 *)

let lex_comment    ctx = lex_until_or_eof ctx (fun c -> c <> '\n')
let lex_identifier ctx = lex_until_or_eof ctx (fun c -> c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')
let lex_number     ctx = lex_until_or_eof ctx (fun c -> c >= '0' && c <= '9' || c = '.')
let lex_colon      ctx = lex_single_char  ctx
let lex_equal      ctx = lex_single_char  ctx
let lex_lbracket   ctx = lex_single_char  ctx
let lex_rbracket   ctx = lex_single_char  ctx
let lex_space      ctx = lex_until_or_eof ctx (fun c -> c = ' ' || c = '\t')
let lex_newline    ctx = lex_until_or_eof ctx (fun c -> c = '\n' || c = '\r')

let lex_string ctx =
  lex_until ctx (fun c -> c <> '\'') ~keep_last: true ~get_token_datum: get_string_token_datum

(*
 ** Entry point.
 *)

let lex ctx =
  reset_lexing_state ctx;
  match peek_char ctx with
  | Some c ->
    begin
      Some(
        match c with
        | '#'                     -> Comment(lex_comment ctx c)
        | '\''                    -> String(lex_string ctx c)
        | 'a' .. 'z' | 'A' .. 'Z' -> Identifier(lex_identifier ctx c)
        | '0' .. '9' | '.'        -> Number(lex_number ctx c)
        | ':'                     -> Colon(lex_colon ctx c)
        | '='                     -> Equal(lex_equal ctx c)
        | '['                     -> LBracket(lex_lbracket ctx c)
        | ']'                     -> RBracket(lex_rbracket ctx c)
        | ' ' | '\t'              -> Space(lex_space ctx c)
        | '\n' | '\r'             -> Newline(lex_newline ctx c)
        | _ -> raise (Error.Error(Lexer_error(Bad_token, ctx.location_current)))
      )
      end
  | None -> None (* EOF *)
