module Tokens = struct
  include Lexer.Tokens
  exception Unexpected_token of string

  let raise token =
    Stdlib.raise (
      Unexpected_token 
      (match token with
      | StructuralChar -> "StructuralChar"
      | String         -> "String"
      | Number         -> "Number"
      | LiteralName    -> "LiteralName")
      ^ ": " ^ to_string token
    )
end

module Numbers = struct
  let of_token number =
    try float_of_string number with Failure message ->
      failwith (message ^ ": " ^ number)
end

module Values = struct
  type t =
  | Object of t StringMap.t
  | Array of t list (* or array? *)
  | Number of float
  | String of string
  | Bool of bool
  | Null

  let of_literal_name = function
  | Lexer.LiteralNames.Bool b -> Bool b
  | Lexer.LiteralNames.Null -> Null

  let of_tokens (token_list : Tokens.t list) : t option =
    match token_list with
    | [] -> None
    | head :: tail ->
    match head with
    | Tokens.Numbers n -> Some (Numbers.of_token n, tail)
    | Tokens.String s -> Some (String s, tail)
    | Tokens.LiteralNames l -> Some (of_literal_name l, tail)
    | Tokens.StructuralChar c ->
    match c with
    | Lexer.StructuralChars.BeginObject -> Some (Objects.of_tokens tail)
    | Lexer.StructuralChars.BeginArray -> Some (Arrays.of_tokens tail)
    | _ -> None
end

module Objects = struct
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
      | Tokens.String key :: tail -> begin
        match Values.of_tokens tail with
        | Some (value, next_token) -> 
        | None -> failwith ("no value for " ^ key)
      end
      
    in
    add token_list StringMap.empty
end

module Arrays = struct
  type bound_token =
  | Begin of Lexer.StructuralChars.BeginArray
  | End of Lexer.StructuralChars.EndArray

  type t = Values.t array

  let of_tokens (token_list : Tokens.t list) : Values.t StringMap.t =
    let rec parse_aux token_list array =
      match token_list with
      | Tokens.StructuralChar c :: tail ->
        Values.of_tokens token_list
    in
    parse_aux token_list []
end

let parse lst =
  let rec structure = function
  | Tokens.StructuralChar c :: tail ->
  | head :: tail -> Tokens.raise head
  | [] ->
  in
