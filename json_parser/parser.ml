exception Open_end of string

let open_end message =
  raise @@ Open_end ("Open " ^ message ^ " at end of file")

module Tokens = struct
  include Lexer.Tokens

  exception Unexpected of string

  let unexpected token =
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

module Numbers = struct
  let of_token number =
    try float_of_string number with Failure message ->
      failwith (message ^ ": " ^ number)
end

module Values = struct
  type t =
  (* | Object of t StringMap.t *)
  | Array of t array (* or list? *)
  | Number of float
  | String of string
  | Bool of bool
  | Null
  | Empty

  let of_literal_name = function
  | Lexer.LiteralNames.Bool b -> Bool b
  | Lexer.LiteralNames.Null -> Null

  let of_tokens (token_list : Tokens.t list) : t * Tokens.t list =
    match token_list with
    | [] -> Empty, []        (* Is an exception warranted here instead? *)
    | head :: tail ->
    match head with
    | Tokens.Number n       -> Number (Numbers.of_token n), tail
    | Tokens.String s       -> String s,           tail
    | Tokens.LiteralName l  -> of_literal_name l,  tail
    | Tokens.StructuralChar c ->
    let value, next_token =
      match c with
      (* | Lexer.StructuralChars.BeginObject -> Objects.of_tokens tail *)
      | Lexer.StructuralChars.BeginArray  -> Arrays.of_tokens  tail
      | _ -> Tokens.unexpected head
    in
    value, next_token
end

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
      | [] -> open_end "object"
      | Tokens.String key :: tail -> begin
        match Values.of_tokens tail with
        | None -> failwith ("no value for " ^ key)
        | Some (value, next_token) -> 
      end
      
    in
    add token_list StringMap.empty
end *)

module Arrays = struct
  (* type bound_token =
  | Begin of Lexer.StructuralChars.BeginArray
  | End of Lexer.StructuralChars.EndArray *)

  type t = Values.t array
  (* type t = Values.t list *)

  let add value array =
    value :: array

  let of_tokens (token_list : Tokens.t list) : Values.t StringMap.t =
    let rec parse_aux token_list rev_array_list =
      match token_list with
      | [] -> open_end "array"
      | Tokens.StructuralChar StructuralChars.EndArray :: tail -> rev_array_list, tail
      | Tokens.StructuralChar StructuralChars.ValueSeparator :: tail -> begin
        match Values.of_tokens tail with
        | Values.Empty, _ -> open_end "array"
        | v, next_token -> parse_aux next_token @@ add v rev_array_list
      end
      | head :: _ -> Tokens.unexpected head
    in
    let rev_array_list, next_token = parse_aux token_list [] in
    let arr = List.rev rev_array_list |> List.to_seq |> Array.of_seq in
    arr, new_token
end

type json = Values.t

let rec parse (lst : Tokens.t list) : json =
  match lst with
  | [] -> Values.Empty
  | _ -> begin
    match Values.of_tokens lst with
    | _, head :: tail -> failwith "unexpected non-whitespace character after JSON data"
    | v, _ -> v
  end

