open Economia_light

let lex_string str =
  let stream = Stream.of_string str in
  let ctx = Lexer.context_of_char_stream stream in
  
  let rec lex lst =
    match Lexer.lex ctx with
    | Some token -> lex (token::lst)
    | None -> List.rev lst
  in
  lex []

let string_of_token_list lst =
  List.fold_left (fun a b -> a ^ "\n" ^ (Lexer.string_of_token b)) "" lst

let test_lex_tokens () =
  let input = "bob \t= 'multi\n\
              line string' someList = [freddy 'fran]k']\n\
              foo bar: baz\n\
              number=. number=.42 number=0.42 number=0..42 number=42. number=42.2\n\
              foo # Some comment\n\
              bar" in
  
  let stream = Stream.of_string input in
  let ctx = Lexer.context_of_char_stream stream in

  let rec lex token_list =
    match Lexer.lex ctx with
    | Some token -> lex (token::token_list)
    | None -> List.rev token_list
  in
  let got = string_of_token_list (lex []) in

  let want : Lexer.token list = [
    Identifier ({value = "bob";                location = {line = 1; column = 0}});
    Space      ({value = " \t";                location = {line = 1; column = 3}});
    Equal      ({value = "=";                  location = {line = 1; column = 12}});
    Space      ({value = " ";                  location = {line = 1; column = 13}});
    String     ({value = "multi\nline string"; location = {line = 1; column = 15}});
    
    Space      ({value = " ";                  location = {line = 2; column = 12}});
    Identifier ({value = "someList";           location = {line = 2; column = 13}});
    Space      ({value = " ";                  location = {line = 2; column = 21}});
    Equal      ({value = "=";                  location = {line = 2; column = 22}});
    Space      ({value = " ";                  location = {line = 2; column = 23}});
    LBracket   ({value = "[";                  location = {line = 2; column = 24}});
    Identifier ({value = "freddy";             location = {line = 2; column = 25}});
    Space      ({value = " ";                  location = {line = 2; column = 31}});
    String     ({value = "fran]k";             location = {line = 2; column = 33}});
    RBracket   ({value = "]";                  location = {line = 2; column = 40}});
    Newline    ({value = "\n";                 location = {line = 2; column = 41}});
    
    Identifier ({value = "foo";                location = {line = 3; column = 0}});
    Space      ({value = " ";                  location = {line = 3; column = 3}});
    Identifier ({value = "bar";                location = {line = 3; column = 4}});
    Colon      ({value = ":";                  location = {line = 3; column = 7}});
    Space      ({value = " ";                  location = {line = 3; column = 8}});
    Identifier ({value = "baz";                location = {line = 3; column = 9}});
    Newline    ({value = "\n";                 location = {line = 3; column = 12}});
    
    Identifier ({value = "number";             location = {line = 4; column = 0}});
    Equal      ({value = "=";                  location = {line = 4; column = 6}});
    Number     ({value = ".";                  location = {line = 4; column = 7}});
    Space      ({value = " ";                  location = {line = 4; column = 8}});
    Identifier ({value = "number";             location = {line = 4; column = 9}});
    Equal      ({value = "=";                  location = {line = 4; column = 15}});
    Number     ({value = ".42";                location = {line = 4; column = 16}});
    Space      ({value = " ";                  location = {line = 4; column = 19}});
    Identifier ({value = "number";             location = {line = 4; column = 20}});
    Equal      ({value = "=";                  location = {line = 4; column = 26}});
    Number     ({value = "0.42";               location = {line = 4; column = 27}});
    Space      ({value = " ";                  location = {line = 4; column = 31}});
    Identifier ({value = "number";             location = {line = 4; column = 32}});
    Equal      ({value = "=";                  location = {line = 4; column = 38}});
    Number     ({value = "0..42";              location = {line = 4; column = 39}});
    Space      ({value = " ";                  location = {line = 4; column = 44}});
    Identifier ({value = "number";             location = {line = 4; column = 45}});
    Equal      ({value = "=";                  location = {line = 4; column = 51}});
    Number     ({value = "42.";                location = {line = 4; column = 52}});
    Space      ({value = " ";                  location = {line = 4; column = 55}});
    Identifier ({value = "number";             location = {line = 4; column = 56}});
    Equal      ({value = "=";                  location = {line = 4; column = 62}});
    Number     ({value = "42.2";               location = {line = 4; column = 63}});
    Newline    ({value = "\n";                 location = {line = 4; column = 67}});
    
    Identifier ({value = "foo";                location = {line = 5; column = 0}});
    Space      ({value = " ";                  location = {line = 5; column = 3}});
    Comment    ({value = "# Some comment";     location = {line = 5; column = 4}});
    Newline    ({value = "\n";                 location = {line = 5; column = 18}});

    Identifier ({value = "bar";                location = {line = 6; column = 0}});
  ]
  in
  let want = string_of_token_list want in
  
  Alcotest.(check string) "Check string repr" want got

let () =
  Alcotest.run "Lexer tests" [
    "suite1", [
      "Lex Tokens", `Quick, test_lex_tokens;
    ];
]
