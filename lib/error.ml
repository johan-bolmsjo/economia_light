let fmt = Printf.sprintf

type lexer_error =
  | Bad_token
  | Unexpected_eof

let string_of_lexer_error = function
  | Bad_token      -> "Bad token"
  | Unexpected_eof -> "Unexpected EOF"

type parser_error =
  | Syntax_error
  | Unknown_variable of string
  | Unknown_parameter of string
  | Duplicate_parameter of string
  | Missing_parameter of string
  | Invalid_argument
  | Unterminated_list

let string_of_parser_error = function
  | Syntax_error              -> "Syntax error"
  | Unknown_variable(name)    -> fmt "Unknown variable '%s'" name
  | Unknown_parameter(name)   -> fmt "Unknown parameter '%s'" name
  | Duplicate_parameter(name) -> fmt "Duplicate parameter '%s'" name
  | Missing_parameter(name)   -> fmt "Missing parameter '%s'" name
  | Invalid_argument          -> "Invalid argument"
  | Unterminated_list         -> "Unterminated list"

type error =
  | Invariant_error of string
  | Lexer_error of lexer_error * Location.t
  | Parser_error of parser_error * Location.t

exception Error of error

let string_of = function
  | Invariant_error(msg) ->
    Printf.sprintf "Invariant error: %s" msg
  | Lexer_error(err, loc) ->
    Printf.sprintf "%s: %s" (string_of_lexer_error err) (Location.string_of loc)
  | Parser_error(err, loc) ->
    Printf.sprintf "%s: %s" (string_of_parser_error err) (Location.string_of loc)
