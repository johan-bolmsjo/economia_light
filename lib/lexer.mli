(** Lexer for Economia Light DSL *)

(** Context used when lexing tokens from some source. *)  
type context

(** Lexed token content *)
type token_datum = {
  value    :string;
  location :Location.t;
}

(** Tokens *)
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

val datum_of_token : token -> token_datum

val string_of_token : token -> string

val context_of_char_stream : char Stream.t -> context

(** [lex ctx] lexes one token from [ctx].
    
    None is returned on end of stream.

    Raises:
      Sys_error
      Error.Error(Lexer_error(...))
*)
val lex : context -> token option
