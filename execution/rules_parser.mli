module JSON : sig
  type value_t = Parser.value_t
  module StringMap = Parser.StringMap
end

module CharMap : sig
	include Map.S with type key = char
end

exception Invalid_struct

val print_invalid_struct: unit -> unit

type action = Left | Right

val action_to_int : action -> int
val action_to_str : action -> string

type transition = {
	read: char;
	to_state: string;
	write: char;
	action: action;
}

type rules = {
	name: string;
	alphabet: string;
	blank: char;
	states: string list;
	initial: string;
	finals: string list;
	transitions: (transition CharMap.t) JSON.StringMap.t;
}

val parse_rules : JSON.value_t -> rules
val validate_rules : rules -> rules
val validate_input : string -> rules -> rules
