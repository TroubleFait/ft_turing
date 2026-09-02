type rules = Rules.rules
type transition = Rules.transition

module CharHash = Utils.CharHash

type machine = {
	rules: rules;
	tape: Tape.t;
	index: int;
	state: string;
	last_change: machine option;
}

val get_transition : machine -> transition

val start_machine : string -> rules -> Tape.t * char
