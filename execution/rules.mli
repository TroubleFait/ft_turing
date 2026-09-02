module JSON : sig
  type value_t = Parser.value_t
  module StringHash = Parser.StringHash
end

module CharHash   = Utils.CharHash
module StringHash = Utils.StringHash

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

type state = transition CharHash.t

type rules = {
	name: string;
	alphabet: string;
	blank: char;
	states: string list;
	initial: string;
	finals: string list;
	transitions: state JSON.StringHash.t;
}

val parse : JSON.value_t -> rules
val validate : rules -> rules
val validate_input : string -> rules -> rules
val is_HALT_reachable: rules -> rules
