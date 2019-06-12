(** All error types and exceptions thrown by Economia Light *)

type lexer_error =
  | Bad_token
  | Unexpected_eof

type parser_error =
  | Syntax_error
  | Unknown_variable of string
  | Unknown_parameter of string
  | Duplicate_parameter of string
  | Missing_parameter of string
  | Invalid_argument
  | Unterminated_list

type error =
  | Invariant_error of string
  | Lexer_error of lexer_error * Location.t
  | Parser_error of parser_error * Location.t

exception Error of error

val string_of : error -> string
