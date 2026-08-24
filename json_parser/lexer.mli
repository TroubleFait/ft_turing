module LiteralNames :
  sig
    type t = Bool of bool | Null
    val scan : string -> int -> (t * int) option
    val to_string : t -> string
  end
module Strings :
  sig
    val scan : string -> int -> (string * int) option
  end
module Numbers :
  sig
    type token = DecimalPoint | Digit19 of char | E | Minus | Plus | Zero
    val token_of : char -> token option
    val is : char -> bool
    val scan : string -> int -> (string * int) option
  end
module StructuralChars :
  sig
    type t = BeginArray | EndArray | NameSeparator | ValueSeparator
    val of_char : char -> t option
    val to_string : t -> string
  end
module Tokens :
  sig
    type t =
        StructuralChar of StructuralChars.t
      | String of string
      | Number of string
      | LiteralName of LiteralNames.t
    val to_string : t -> string
  end
val skip_whitespaces : string -> int -> int -> int
val read_through : string -> int -> int -> Tokens.t list
val lex : string -> Tokens.t list
