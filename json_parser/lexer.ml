module LiteralNames = struct
  type t =
  | Bool of bool
  | Null

  let true_len  = String.length "true"
  let false_len = String.length "false"
  let null_len  = String.length "null"

  let scan str start : (t * int) option =
    match (String.length str) - start with
    | len when len >= true_len  && String.sub str start true_len  = "true"  -> Some @@ Bool true,  start + true_len
    | len when len >= false_len && String.sub str start false_len = "false" -> Some @@ Bool false, start + false_len
    | len when len >= null_len  && String.sub str start null_len  = "null"  -> Some @@ Null,       start + null_len
    | _ -> None

  let to_string = function
  | Bool b -> string_of_bool b
  | Null   -> "null"
end

module Strings = struct
  exception Malformed of string

  let raise_malformed msg = raise Malformed @@ "malformed string: " ^ msg

  let string_of_utf_8_char my_char =
    let buff = Buffer.create 4 in
    Buffer.add_utf_8_uchar buff my_char;
    Buffer.contents buff

  let int_of_sequence s i =
    ("0x" ^ (String.sub s i 4)) |> int_of_string

  let utf_8_of_utf_16_surrogate h l =
    (h - 0xD800) * 0x400 + (l - 0xDC00) + 0x10000

  let unescape_sequence s i =
    let len = String.length s in
    if i + 4 >= len then
      raise_malformed @@ "incomplete escape sequence: `" ^ (String.sub s (i - 2) (len - i + 2)) ^ "'"
    else begin
      let seq_int, end_pos =
        if len > i + 10 && String.sub s (i + 4) 2 = "\\u" then
          (utf_8_of_utf_16_surrogate (int_of_sequence s i) (int_of_sequence s @@ i + 6)), i + 10
        else
          (int_of_sequence s i), i + 4
      in
      (seq_int |> Uchar.of_int |> string_of_utf_8_char), end_pos
    end

  let unescape_char s i =
    match s.[i] with
    | '"' | '\\' | '/' -> Utils.string_to_char s.[i], (i + 1)
    | 'b' ->    "\b", (i + 1)
    | 'f' ->  "\x0C", (i + 1)
    | 'n' ->    "\n", (i + 1)
    | 'r' ->    "\r", (i + 1)
    | 't' ->    "\t", (i + 1)
    | 'u' -> unescape_sequence s (i + 1)
    |  _  -> raise_malformed ("invalid escape character: `\\" ^ Utils.string_to_char s.[i] ^ "'")

  let scan str start : (string * int) option =
    if str.[start] <> '"' then
      None
    else begin
      let len = String.length str in
      let rec go acc = function
      | i when i >= len     -> raise_malformed "no ending quote"
      | i when str.[i] = '"'  -> acc |> List.rev |> String.concat "", i + 1
      | i when str.[i] = '\\' -> if (i + 1) >= len then
          raise_malformed "incomplete escape sequence: `\\'"
        else 
          let c, next_pos = unescape_char str (i + 1) in
          go (c :: acc) next_pos
      | i -> go (Utils.string_to_char str.[i] :: acc) (i + 1)
      in
      Some @@ go [] (start + 1)
    end
end

module Numbers = struct
  exception Malformed of string

  let raise_malformed msg = raise Malformed @@ "malformed number: " ^ msg

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
  
  let scan str start : (string * int) option =
    if not @@ is str.[start] then
      None
    else begin
      let find_end_digits start =
        String.find_first_index (fun c -> match token_of c with
            | Some Zero | Some (Digit19 _) -> false
            | _ -> true)
          ~start
          str
      in
      let int_end int_start = begin match token_of str.[int_start] with
        | Some Minus -> int_start + 1
        | _ -> int_start
        end |> find_end_digits
      in
      let frac_end frac_start = match token_of str.[frac_start] with
        | Some DecimalPoint -> begin
          match token_of str.[frac_start + 1] with
          | Some Zero | Some (Digit19 _) -> find_end_digits (frac_start + 1)
          | _ -> raise_malformed @@ "unterminated fractional part: `" ^ String.sub str start (frac_start - start) ^ "'"
        end
        | _ -> Some frac_start
      in
      let exp_end exp_start = match token_of str.[exp_start] with
        | Some E -> begin
          match token_of str.[exp_start + 1] with
          | Some Plus | Some Minus -> begin
	          match token_of str.[exp_start + 2] with
	          | Some Zero | Some (Digit19 _) -> find_end_digits (exp_start + 2)
            | _ -> raise_malformed @@ "exponent part is missing a number: `" ^ String.sub str start (exp_start - start) ^ "'"
	        end
          | Some Zero | Some (Digit19 _) -> find_end_digits (exp_start + 1)
          | _ -> raise_malformed @@ "exponent part is missing a number: `" ^ String.sub str start (exp_start - start) ^ "'"
        end
        | _ -> Some exp_start
      in

			let number_end =
				match int_end start with
				| None -> String.length str
				| Some v ->
				match frac_end v with
				| None -> String.length str
				| Some v ->
				match exp_end v with
				| None -> String.length str
				| Some v -> v
			in
			Some (String.sub str start (number_end - start), number_end)
    end
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
  | LiteralName of LiteralNames.t

  let to_string = function
  | StructuralChar c -> StructuralChars.to_string c
  | String s         -> "\"" ^ String.escaped s ^ "\""
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
