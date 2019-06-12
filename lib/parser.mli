(** Parser for Economia Light DSL *)

(* Parser state that is produced anew for each parsed token. *)
type state

(** Initial parser state. *)
val init_state : state

(** Parse token and produce new parser state.

    Raises:
      Error.Error(Parser_error(...))
*)
val parse: Lexer.token -> state -> state

(** Complete parsing in the event the last token was not a statemen terminating token.
    
    Raises:
      Error.Error(Parser_error(...))
*)
val complete: state -> state

(** Reports string representation of state. *)
val string_of_state: state -> string
