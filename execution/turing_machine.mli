type action_t = Left | Right

val action_to_int : action_t -> int
val action_to_str : action_t -> string

type transition_t = {
	read: string;
	to_state: string;
	write: string;
	action: action_t;
}

type dict_t = {
	name: string;
	alphabet: string list;
	blank: string;
	states: string list;
	initial: string;
	finals: string list;
	transitions: (string * (transition_t list)) list;
}

type machine_t = {
	dict: dict_t;
	tape: string;
	index: int;
	state: string;
}

val print_tape : machine_t -> unit
val print_step : machine_t -> transition_t -> unit

val get_transition_single : machine_t -> transition_t list -> transition_t
val get_transition_lst    : machine_t -> transition_t list
val get_transition        : machine_t -> transition_t

val execute_cell : machine_t -> machine_t

val start_machine : string -> string
