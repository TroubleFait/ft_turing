module StringMap :
  sig
    type key = String.t
    type 'a t = 'a Map.Make(String).t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding : 'a t -> key * 'a
    val max_binding_opt : 'a t -> (key * 'a) option
    val choose : 'a t -> key * 'a
    val choose_opt : 'a t -> (key * 'a) option
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val is_empty : 'a t -> bool
    val is_singleton : 'a t -> bool
    val mem : key -> 'a t -> bool
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val to_list : 'a t -> (key * 'a) list
    val of_list : (key * 'a) list -> 'a t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_rev_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

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
  | Object of value_t StringMap.t
  | Array of value_t array
  | Number of float
  | String of string
  | Bool of bool
  | Null
  | Empty
val value_of_literal_name : Lexer.LiteralNames.t -> value_t
val value_of_tokens : Tokens.t list -> value_t * Tokens.t list

type object_t = value_t StringMap.t
val object_add : string -> value_t -> object_t -> object_t
val object_of_tokens : Tokens.t list -> object_t * Tokens.t list

type array_t = value_t array
val array_of_tokens : Tokens.t list -> array_t * Tokens.t list

type json = value_t
val parse : Tokens.t list -> json
