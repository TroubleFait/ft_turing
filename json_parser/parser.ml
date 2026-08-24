module StringMap = Map.Make(String)

exception Open_end of string

let raise_open_end message =
  raise @@ Open_end ("Open " ^ message ^ " at end of file")

module Tokens = struct
  include Lexer.Tokens

  exception Unexpected of string

  let raise_unexpected token =
    raise (
      Unexpected (
      (match token with
      | StructuralChar _ -> "StructuralChar"
      | String _         -> "String"
      | Number _         -> "Number"
      | LiteralName _    -> "LiteralName")
      ^ ": " ^ to_string token)
    )
end

(* module Numbers = struct *)
  let number_of_token number =
    try float_of_string number with Failure message ->
      failwith (message ^ ": " ^ number)
(* end *)

(* module Values = struct *)
  type value_t =
  (* | Object of t StringMap.t *)
  | Array of value_t array (* or list? *)
  | Number of float
  | String of string
  | Bool of bool
  | Null
  | Empty

  let value_of_literal_name = function
  | Lexer.LiteralNames.Bool b -> Bool b
  | Lexer.LiteralNames.Null -> Null

  (* (* See below, near array_of_token *)
  let value_of_tokens (token_list : Tokens.t list) : value_t * Tokens.t list =
    match token_list with
    | [] -> Empty, []        (* Is an exception warranted here instead? *)
    | head :: tail ->
    match head with
    | Tokens.Number n       -> Number (number_of_token n), tail
    | Tokens.String s       -> String s,           tail
    | Tokens.LiteralName l  -> value_of_literal_name l,  tail
    | Tokens.StructuralChar c ->
    let value, next_token =
      match c with
      (* | Lexer.StructuralChars.BeginObject -> Objects.of_tokens tail *)
      | Lexer.StructuralChars.BeginArray  -> array_of_tokens  tail
      | _ -> Tokens.raise_unexpected head
    in
    value, next_token *)
(* end *)

(* module Objects = struct
  type bound_token =
  | Begin of Lexer.StructuralChars.BeginObject
  | End of Lexer.StructuralChars.EndObject

  module StringMap = Map.Make(String)

  type t = Values.t StringMap.t

  let add_value name lst =
    match lst with
    |  -> pattern

  let of_tokens (token_list : Tokens.t list) : Values.t StringMap.t =
    let rec add token_list map =
      match token_list with
      | [] -> raise_open_end "object"
      | Tokens.String key :: tail -> begin
        match Values.of_tokens tail with
        | None -> failwith ("no value for " ^ key)
        | Some (value, next_token) -> 
      end
      
    in
    add token_list StringMap.empty
end *)

(* module Arrays = struct *)
  (* type bound_token =
  | Begin of Lexer.StructuralChars.BeginArray
  | End of Lexer.StructuralChars.EndArray *)

  type array_t = value_t array
  (* type t = Values.t list *)

  let array_add value array =
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
    (* | Lexer.StructuralChars.BeginObject -> let value, next_token = object_of_tokens tail in Object (value), next_token *)
    | Lexer.StructuralChars.BeginArray  -> let value, next_token = array_of_tokens  tail in Array  (value), next_token
    | _ -> Tokens.raise_unexpected head

  (* and array_of_tokens (token_list : Tokens.t list) : value_t StringMap.t = *)
  and array_of_tokens (token_list : Tokens.t list) : array_t * Tokens.t list =
    let rec parse_aux token_list rev_array_list =
      match token_list with
      | [] -> raise_open_end "array"
      | Tokens.StructuralChar Lexer.StructuralChars.EndArray :: tail -> rev_array_list, tail
      | _ ->
      match value_of_tokens token_list with
      | Empty, _ -> raise_open_end "array"
      | value, next_token ->
      match next_token with
      | Tokens.StructuralChar Lexer.StructuralChars.ValueSeparator :: tail -> parse_aux tail @@ array_add value rev_array_list
      | Tokens.StructuralChar Lexer.StructuralChars.EndArray :: tail -> array_add value rev_array_list, tail
      | [] -> raise_open_end "array"
      | head :: _ -> Tokens.raise_unexpected head
    in
    let rev_array_list, next_token = parse_aux token_list [] in
    let arr = List.rev rev_array_list |> List.to_seq |> Array.of_seq in
    arr, next_token
(* end *)

type json = value_t

let rec parse (lst : Tokens.t list) : json =
  match lst with
  | [] -> Empty
  | _ -> 
  match value_of_tokens lst with
  | _, head :: tail -> failwith "unexpected non-whitespace character after JSON data"
  | v, _ -> v
  (* À revoir *)
