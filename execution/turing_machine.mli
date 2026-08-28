type rules = Rules_parser.rules
type transition = Rules_parser.transition

module CharMap = Rules_parser.CharMap

type machine = {
	rules: rules;
	tape: string;
	index: int;
	state: string;
	last_change: machine option;
}

val tape_to_str   : ?window_size:int -> machine -> transition -> string
val print_tape    : ?window_size:int -> machine -> transition -> unit
val print_step    : ?window_size:int -> machine -> transition -> unit

val get_transition : machine -> transition

val start_machine : string -> rules -> string
