type rules = Rules_parser.rules
type transition = Rules_parser.transition

module CharHash = Rules_parser.CharHash

type machine = {
	rules: rules;
	tape: string;
	index: int;
	state: string;
	last_change: machine option;
}

val get_transition : machine -> transition

val start_machine : string -> rules -> string
