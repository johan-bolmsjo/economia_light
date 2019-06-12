open Economia_light

let with_lexer_debug   = false
let with_exn_backtrace = false (* Set environment variable OCAMLRUNPARAM=b as well *)

let exn_backtrace() =
  (if with_exn_backtrace then Printexc.print_backtrace stdout)

(* Parse content of file and return a final transaction summary. *)
let parse_file (filename :string) : string =
  let ichan = open_in filename in
  let lex_ctx = Lexer.context_of_char_stream (Stream.of_channel ichan) in
  let rec lex pstate =
    match Lexer.lex lex_ctx with
    | Some token ->
      (if with_lexer_debug then print_endline (Lexer.string_of_token token));
      lex (Parser.parse token pstate)
    | None -> pstate
  in
  let pstate = Parser.complete (lex Parser.init_state) in
  Parser.string_of_state pstate

let () =
  let fail msg =
    print_endline msg;
    exit 1
  in
  match (List.tl (Array.to_list Sys.argv)) with
  | filename :: [] ->
    begin
      try
        Printf.printf "%s" (parse_file filename)
      with
      | Error.Error(err) -> (exn_backtrace()); fail (Error.string_of err)
      | Sys_error(msg)   -> (exn_backtrace()); fail msg
    end
  | _ -> fail (Printf.sprintf "usage: %s INPUT_FILE" Sys.argv.(0))
