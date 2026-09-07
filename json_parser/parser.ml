module StringHash = Utils.StringHash

exception Open_end of string

let raise_open_end message =
  raise @@ Open_end (Printf.sprintf "Open %s at end of file" message)

module Tokens = struct
  include Lexer.Tokens

  exception Unexpected of string

  let raise_unexpected token =
    raise @@ Unexpected ("Unexpected Token: " ^ (
      match token with
      | StructuralChar _ -> "StructuralChar"
      | String _         -> "String"
      | Number _         -> "Number"
      | LiteralName _    -> "LiteralName"
      ) ^ ": " ^ to_string token)
end

let number_of_token number =
  try float_of_string number with Failure message ->
    failwith @@ message ^ ": " ^ number

type value_t =
| Object of value_t StringHash.t
| Array of value_t array
| Number of float
| String of string
| Bool of bool
| Null
| Empty

let value_of_literal_name = function
| Lexer.LiteralNames.Bool b -> Bool b
| Lexer.LiteralNames.Null -> Null


type object_t = value_t StringHash.t

module StringMap = Map.Make(String)

let object_add key value obj =
  StringMap.add key value obj


type array_t = value_t array

let rev_array_list_add value array =
  value :: array


let rec value_of_tokens (token_list : Tokens.t list) : value_t * Tokens.t list =
  match token_list with
  | [] -> Empty, []
  | head :: tail ->
  match head with
  | Tokens.Number n         -> Number (number_of_token n), tail
  | Tokens.String s         -> String s,                   tail
  | Tokens.LiteralName l    -> value_of_literal_name l,    tail
  | Tokens.StructuralChar c ->
  match c with
  | Lexer.StructuralChars.BeginObject -> let value, next_token = object_of_tokens tail in Object (value), next_token
  | Lexer.StructuralChars.BeginArray  -> let value, next_token = array_of_tokens  tail in Array  (value), next_token
  | _ -> Tokens.raise_unexpected head

and object_of_tokens (token_list : Tokens.t list) : object_t * Tokens.t list =
  let rec parse token_list obj_map =
    match token_list with
    | [] -> raise_open_end "object"
    | Tokens.StructuralChar Lexer.StructuralChars.EndObject :: tail -> obj_map, tail
    | Tokens.String key :: tail -> begin
      match tail with
      | Tokens.StructuralChar Lexer.StructuralChars.NameSeparator :: tail -> begin
        match value_of_tokens tail with
        | Empty, _ -> raise_open_end "object"
        | value, next_token ->
        match next_token with
        | Tokens.StructuralChar Lexer.StructuralChars.EndObject :: tail -> object_add key value obj_map, tail
        | Tokens.StructuralChar Lexer.StructuralChars.ValueSeparator :: tail -> parse tail @@ object_add key value obj_map
        | [] -> raise_open_end "object"
        | head :: _ -> Tokens.raise_unexpected head
      end
      | [] -> raise_open_end "object"
      | head :: _ -> Tokens.raise_unexpected head
    end
    | head :: _ -> Tokens.raise_unexpected head
  in
  let obj_map, next_token = parse token_list StringMap.empty in
  let obj = obj_map |> StringMap.to_seq |> StringHash.of_seq in
  obj, next_token

and array_of_tokens (token_list : Tokens.t list) : array_t * Tokens.t list =
  let rec parse token_list rev_array_list =
    match token_list with
    | [] -> raise_open_end "array"
    | Tokens.StructuralChar Lexer.StructuralChars.EndArray :: tail -> rev_array_list, tail
    | _ ->
    match value_of_tokens token_list with
    | Empty, _ -> raise_open_end "array"
    | value, next_token ->
    match next_token with
    | Tokens.StructuralChar Lexer.StructuralChars.EndArray :: tail -> rev_array_list_add value rev_array_list, tail
    | Tokens.StructuralChar Lexer.StructuralChars.ValueSeparator :: tail -> parse tail @@ rev_array_list_add value rev_array_list
    | [] -> raise_open_end "array"
    | head :: _ -> Tokens.raise_unexpected head
  in
  let rev_array_list, next_token = parse token_list [] in
  let arr = rev_array_list |> List.rev |> List.to_seq |> Array.of_seq in
  arr, next_token

type json = value_t

let rec parse (lst : Tokens.t list) : json =
  match lst with
  | [] -> Empty
  | _ -> 
  match value_of_tokens lst with
  | _, head :: _ -> Tokens.raise_unexpected head
  | v, _ -> v
