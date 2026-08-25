module LiteralNames = struct
  type t =
  | Bool of bool
  | Null

  let scan str start : (t * int) option =
    let starts_with sub =
      let len = String.length sub in
      let rec loop i =
        if i >= len then true
        else if str.[start + i] <> sub.[i] then false
        else loop (i + 1)
      in
      loop 0
    in
    if starts_with "false" then
      Some ((Bool false), start + String.length "false")
    else if starts_with "null" then
      Some (Null, start + String.length "null")
    else if starts_with "true" then
      Some ((Bool true), start + String.length "true")
    else
      None

  let to_string = function
  | Bool b -> string_of_bool b
  | Null   -> "null"
end

module Strings = struct     (* TODO: remove quotes around string, un-escape quotes inside, un-escape other characters like '\n'? *)
  let scan str start : (string * int) option =
    if str.[start] <> '"' then
      None
    else begin
      let len = String.length str in
      let is_escaped pos =
        let rec count_escape_chars pos acc =
          if str.[pos - 1] = '\\' then
            count_escape_chars (pos - 1) (acc + 1)
          else acc
        in
        (count_escape_chars pos 0) mod 2 <> 0
      in
      let rec find_end_quote from =
        if from >= len then None else
        match String.index_from_opt str from '"' with
        | None -> None
        | Some v when is_escaped v -> find_end_quote (v + 1)
        | Some v -> Some v
      in
      let end_pos =
        match find_end_quote (start + 1) with
        | Some v -> v + 1
        | None -> failwith "malformed string: no ending quote"
      in
      Some (String.sub str start (end_pos - start), end_pos)
    end
end

module Numbers = struct
  type token =
  | DecimalPoint
  | Digit19 of char
  | E
  | Minus
  | Plus
  | Zero
  
  let token_of c = match c with
  | '.'       -> Some DecimalPoint
  | '1'..'9'  -> Some (Digit19 c)
  | 'e' | 'E' -> Some E
  | '-'       -> Some Minus
  | '+'       -> Some Plus
  | '0'       -> Some Zero
  |  _        -> None

  let is c = match token_of c with
  | Some Minus | Some Zero | Some Digit19 _ -> true
  | _ -> false

  (* type t = {
    (* int   : string;
    frac  : string; *)
    num : string;
    exp : string;
  } *)
  
  let scan str start : (string * int) option =
    if not @@ is str.[start] then
      None
    else begin
      let find_end_digits start =
        match String.find_first_index (fun c -> match token_of c with
            | Some Zero | Some (Digit19 _) -> false
            | _ -> true)
          ~start
          str
        with Some v -> v | None -> failwith "truncated file"
      in
      let int_end = begin match token_of str.[start] with Some Minus -> start + 1 | _ -> start end
        |> find_end_digits
      in
      let frac_end = match token_of str.[int_end] with
        | Some DecimalPoint -> begin
          match token_of str.[int_end + 1] with
          | Some Zero | Some (Digit19 _) -> find_end_digits (int_end + 1)
          | _ -> failwith ("unterminated fractional number: `" ^ String.sub str start (int_end - start) ^ "'")
        end
        | _ -> int_end
      in
      let exp_end = match token_of str.[frac_end] with
        | Some E -> begin
          match token_of str.[frac_end + 1] with
          | Some Zero | Some (Digit19 _) -> find_end_digits (frac_end + 1)
          | _ -> failwith ("exponent part is missing a number: `" ^ String.sub str start (frac_end - start) ^ "'")
        end
        | _ -> frac_end
      in
      (* Some ({
        (* int  = String.sub str start int_end;
        frac = String.sub str (int_end + 1) frac_end; *)
        num = String.sub str start frac_end;
        exp = String.sub str (frac_end + 1) exp_end;
      }, exp_end + 1) *)
      Some (String.sub str start (exp_end - start), exp_end)
    end

  (* let to_string n =
    n.int
    ^ if String.is_empty n.frac then "" else ("." ^ n.frac)
    ^ if String.is_empty n.exp  then "" else ("e" ^ n.exp) *)
end

module StructuralChars = struct
  type t =
  | BeginArray
  | EndArray
  | BeginObject
  | EndObject
  | NameSeparator
  | ValueSeparator

  let of_char =
    function
  | '[' -> Some BeginArray
  | ']' -> Some EndArray
  | '{' -> Some BeginObject
  | '}' -> Some EndObject
  | ':' -> Some NameSeparator
  | ',' -> Some ValueSeparator
  |  _  -> None

  let to_string = function
  | BeginArray     -> "["
  | EndArray       -> "]"
  | BeginObject    -> "{"
  | EndObject      -> "}"
  | NameSeparator  -> ":"
  | ValueSeparator -> ","
end

module Tokens = struct
  type t =
  | StructuralChar of StructuralChars.t
  | String of string
  | Number of string
  (* | Number of Numbers.t *)
  | LiteralName of LiteralNames.t

  let to_string = function
  | StructuralChar c -> StructuralChars.to_string c
  | String s         -> "\"" ^ String.escaped s ^ "\""
  (* | Number n         -> Numbers.to_string n *)
  | Number n         -> n
  | LiteralName l    -> LiteralNames.to_string l
end

let skip_whitespaces str len start =
  let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false in
  match String.find_first_index (fun c -> not @@ is_whitespace c) ~start str with
  | Some stop -> stop
  | None -> len

let rec read_through str len start =
  if start >= len then [] else
  let new_token, new_pos =
    match StructuralChars.of_char str.[start] with
    | Some c -> Tokens.StructuralChar c, (start + 1)
    | None ->
    match LiteralNames.scan str start with
    | Some (l, next_pos) -> Tokens.LiteralName l, next_pos
    | None ->
    match Strings.scan str start with
    | Some (s, next_pos) -> Tokens.String s, next_pos
    | None ->
    match Numbers.scan str start with
    | Some (n, next_pos) -> Tokens.Number n, next_pos
    | None ->
    failwith ("invalid token: `" ^ String.sub str start (1) ^ "'")
  in
  new_token :: read_through str len (skip_whitespaces str len new_pos)

let lex str =
  let len = String.length str in
  read_through str len (skip_whitespaces str len 0)
