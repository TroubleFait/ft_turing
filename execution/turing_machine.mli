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
	transitions: (string * (transition list)) list;
}

type machine = {
	rules: rules;
	tape: string;
	index: int;
	state: string;
}

val tape_to_str   : ?window_size:int -> machine -> string
val print_tape    : ?window_size:int -> machine -> unit
val print_step    : ?window_size:int -> machine -> transition -> unit

val get_transition_single : machine -> transition list -> transition
val get_transition_lst    : machine -> transition list
val get_transition        : machine -> transition

val execute_cell : machine -> machine

val start_machine : string -> string
