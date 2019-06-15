module String_map = Map.Make(String)

let location_of_token token = (Lexer.datum_of_token token).location

let invariant_error msg =
  Error.Error(Invariant_error(msg))

let syntax_error location =
  Error.Error(Parser_error(Syntax_error, location))

let unknown_variable_error location name =
  Error.Error(Parser_error(Unknown_variable(name), location))

let unknown_parameter_error location name =
  Error.Error(Parser_error(Unknown_parameter(name), location))

let duplicate_parameter_error location name =
  Error.Error(Parser_error(Duplicate_parameter(name), location))

let missing_parameter_error location name =
  Error.Error(Parser_error(Missing_parameter(name), location))

let invalid_argument_error location =
  Error.Error(Parser_error(Invalid_argument, location))

let unterminated_list_error location =
  Error.Error(Parser_error(Unterminated_list, location))

(* Variable or function parameter value *)
module Value = struct
  type t =
    | Number of float * Location.t
    | String of string * Location.t
    | List of t list * Location.t

  let location = function
    | Number(_, loc) | String(_, loc) | List(_, loc) -> loc
  
  (* Parse value from a list of token.
     Any identifier is resolved as a variable by lookup in [vars].

     Remaining tokens are returned together with the parsed value.
  *)
  let of_tokens (vars : t String_map.t) (tokens : Lexer.token list) : (t * Lexer.token list) =
    let number_of_datum (datum : Lexer.token_datum) =
      try Number(float_of_string datum.value, datum.location)
      with _ -> raise (syntax_error datum.location)
    in
    let value_of_var (datum : Lexer.token_datum) =
      try String_map.find datum.value vars
      with _ -> raise (unknown_variable_error datum.location datum.value)
    in
    let rec parse_list (acc : t list) (list_location : Location.t) (tokens : Lexer.token list) : (t * Lexer.token list) =
      match tokens with
      | LBracket(x)::xs ->
        begin
          let (value, remain) = parse_list [] x.location xs in
          parse_list (value::acc) list_location remain
        end
      | RBracket(_)::xs -> (List(List.rev acc, list_location), xs)
      | [] -> raise (unterminated_list_error list_location)
      | xs ->
        begin
          let (value, remain) = parse_one xs in
          parse_list (value::acc) list_location remain
        end
    and parse_one (tokens : Lexer.token list) : (t * Lexer.token list) =
      match tokens with
      | Lexer.String(x)::xs     -> (String(x.value, x.location), xs)
      | Lexer.Number(x)::xs     -> ((number_of_datum x), xs)
      | Lexer.Identifier(x)::xs -> ((value_of_var x), xs)
      | x::_                    -> raise (syntax_error (location_of_token x))
      | []                      -> raise (invariant_error "Value.of_tokens given empty list")
    in
    match tokens with
    | LBracket(x)::xs -> parse_list [] x.location xs
    | xs              -> parse_one xs
end

(* Payment statement argument *)
module Payment_arg = struct
  type t =
    | By of (string * Location.t)
    | For of (string list * Location.t)
    | Amount of (float * Location.t)
    | Rate of (float * Location.t)
    | What of (string * Location.t)
 
  let accept_number = function
    | Value.Number(num, loc) -> (num, loc)
    | x -> raise (invalid_argument_error (Value.location x))

  let accept_string = function
    | Value.String(str, loc) -> (str, loc)
    | x -> raise (invalid_argument_error (Value.location x))

  let accept_string_list = function
    | Value.List(lst, loc) ->
      begin
        let rec collect (acc : string list) = function
          | Value.String(str, _)::xs -> collect (str::acc) xs
          | x::_ -> raise (invalid_argument_error (Value.location x))
          | [] -> (List.rev acc, loc)
        in
        collect [] lst
      end
    | x -> raise (invalid_argument_error (Value.location x))

  let parse (param : Lexer.token_datum) (value : Value.t) : t =
    match param.value with
    | "by"     -> By(accept_string value)
    | "for"    -> For(accept_string_list value)
    | "amount" -> Amount(accept_number value)
    | "rate"   -> Rate(accept_number value)
    | "what"   -> What(accept_string value)
    | _ -> raise (unknown_parameter_error param.location param.value)
    
end

type state = {
  num  : int;                   (* Transaction number *)
  vars : Value.t String_map.t;  (* Variables *)
  bal  : float String_map.t;    (* All users current balance *)
  bt   : Lexer.token list;      (* Token backtracking information *)
}

let init_state = {
  num  = 0;
  vars = String_map.empty;
  bal  = String_map.empty;
  bt   = [];
}


let parse_payment (receiver : Lexer.token_datum) (tokens : Lexer.token list) state : state =

  let rec collect acc = function
    | Lexer.Identifier(name)::Colon(colon)::xs ->
      let value, remain =
        if List.length xs == 0 then raise (syntax_error colon.location)
        else Value.of_tokens state.vars xs
      in
      collect ((name, (Payment_arg.parse name value))::acc) remain
    | x::_ -> raise (syntax_error (Lexer.datum_of_token x).location)
    | [] -> List.rev acc
  in

  (* Check for duplicate parameter names and collect arguements in a map by name *)
  let args =
    List.fold_left (fun acc (elt : (Lexer.token_datum * Payment_arg.t)) ->
        let (name, arg) = elt in
        match String_map.find_opt name.value acc with
        | Some _ -> raise (duplicate_parameter_error name.location name.value)
        | None -> String_map.add name.value arg acc
      ) String_map.empty (collect [] tokens)
  in

  (* These accessors are not fluid.

     Should rethink data structures but this is just a learning experience and I
     just want to finish this boring program now. I have learnt some by coming
     this far.

     Onward to new challenges!
  *)
  
  let get_by() : string =
    let param = "by" in
    match String_map.find_opt param args with
    | Some(By(v, _)) -> v
    | Some _ | None -> raise (missing_parameter_error receiver.location param)
  in

  let get_for() : string list =
    let param = "for" in
    let v, loc = match String_map.find_opt param args with
      | Some(For(v, loc)) -> v, loc
      | Some _ | None -> raise (missing_parameter_error receiver.location param)
    in
    if (List.length v) == 0 then raise (invalid_argument_error loc)
    else v
  in

  let get_amount() : float =
    let param = "amount" in
    match String_map.find_opt param args with
    | Some(Amount(v, _)) -> v
    | Some _ | None -> raise (missing_parameter_error receiver.location param)
  in
  
  let get_rate() : float =
    let param = "rate" in
    match String_map.find_opt param args with
    | Some(Rate(v, _)) -> v
    | Some _ | None -> 1.0 (* sensible default *)
  in

  let amount = get_amount() *. get_rate() in

  (* Add the whole amount to the balance of the one who payed. *)
  let bal = String_map.update (get_by()) (function
      | Some(v) -> Some(v +. amount)
      | None -> Some(amount)
    ) state.bal
  in

  let sharing = get_for() in
  let amount = amount /. (float_of_int (List.length sharing)) in

  (* Split the among all the participants. *)
  let bal =
    List.fold_left (fun bal elt ->
        String_map.update elt (function
            | Some(v) -> Some(v -. amount)
            | None -> Some(-.amount)
          ) bal
      ) bal sharing
  in

  {state with num = state.num + 1; bal}


let parse_assign (receiver : Lexer.token_datum) (tokens : Lexer.token list) state : state =
  if List.length tokens == 0 then raise (syntax_error receiver.location)
  else
    let (value, remain) = Value.of_tokens state.vars tokens in
    match remain with
    | x::_ -> raise (syntax_error (location_of_token x)) (* trailing garbage *)
    | _    -> { state with vars = String_map.add receiver.value value state.vars }

let parse_line (tokens : Lexer.token list) state : state =
  match tokens with
  | Identifier(car)::cdr ->
    begin
      match cdr with
      | Equal(_)::cddr -> parse_assign car cddr state
      | _ ->
        if car.value = "payment" then
          parse_payment car cdr state
        else
          raise (syntax_error car.location)
    end
  | car::_ -> raise (syntax_error (location_of_token car))
  | [] -> state

let parse (token : Lexer.token) state : state =
  match token with
  | Space _ | Comment(_) -> state (* Parsing does not rely on these tokens *)
  | Newline _ -> parse_line (List.rev state.bt) {state with bt = []}
  | _ -> { state with bt = token::state.bt } (* Hold on to token until a newline is found *)

let complete state : state =
  match state.bt with
  | [] -> state
  | xs -> parse_line (List.rev xs) {state with bt = []}

let string_of_state state : string =
  let fmt = Printf.sprintf in
  Seq.fold_left (fun acc elt ->
      acc ^ (let (a, b) = elt in fmt "%s: %.2f\n" a b)
    ) (fmt "# Transaction %d\n" state.num) (String_map.to_seq state.bal)
