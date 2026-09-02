module StringHash = Utils.StringHash

exception Open_end of string

val raise_open_end : string -> 'a

module Tokens :
  sig
    type t =
      Lexer.Tokens.t =
      | StructuralChar of Lexer.StructuralChars.t
      | String of string
      | Number of string
      | LiteralName of Lexer.LiteralNames.t
    val to_string : t -> string
    exception Unexpected of string
    val raise_unexpected : t -> 'a
  end

val number_of_token : string -> float

type value_t =
  | Object of value_t StringHash.t
  | Array of value_t array
  | Number of float
  | String of string
  | Bool of bool
  | Null
  | Empty
val value_of_literal_name : Lexer.LiteralNames.t -> value_t
val value_of_tokens : Tokens.t list -> value_t * Tokens.t list

type object_t = value_t StringHash.t
val object_add : string -> value_t -> object_t -> object_t
val object_of_tokens : Tokens.t list -> object_t * Tokens.t list

type array_t = value_t array
val array_of_tokens : Tokens.t list -> array_t * Tokens.t list

type json = value_t
val parse : Tokens.t list -> json
