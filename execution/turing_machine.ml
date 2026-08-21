let char_to_string c = String.make 1 c

let print_err fmt =
	Printf.printf "%!";
	Printf.eprintf ("\027[31m" ^^ fmt ^^ "\027[0m")

type action = Left | Right

let action_to_int = function
	| Left -> -1
	| Right -> 1

let action_to_str = function
  | Left -> "LEFT"
  | Right -> "RIGHT"

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

let dicti = {
	name = "unary_sub";
	alphabet= "1.-=";
	blank = '.';
	states = [ "scanright"; "eraseone"; "subone"; "skip"; "HALT" ];
	initial = "scanright";
	finals = [ "HALT" ];
	transitions = [
		("scanright", [
			{ read= '.'; to_state= "scanright"; write= '.'; action= Right};
			{ read= '1'; to_state= "scanright"; write= '1'; action= Right};
			{ read= '-'; to_state= "scanright"; write= '-'; action= Right};
			{ read= '='; to_state= "eraseone" ; write= '.'; action= Left }
		]);
		("eraseone", [
			{ read= '1'; to_state= "subone"; write= '='; action= Left};
			{ read= '-'; to_state= "HALT" ; write= '.'; action= Left}
		]);
		("subone", [
			{ read= '1'; to_state= "subone"; write= '1'; action= Left};
			{ read= '-'; to_state= "skip" ; write= '-'; action= Left}
		]);
		("skip", [
			{ read= '.'; to_state= "skip" ; write= '.'; action= Left};
			{ read= '1'; to_state= "scanright"; write= '.'; action= Right}
		])
	]
}

let tape_to_str ?(window_size = 20) (machine: machine) : string =
	let center = (window_size - 1) / 2 in
	let tape_len = String.length machine.tape in
	let symbol_i i =
		let tape_pos = machine.index + i - (center - (if i < center - 1 then 1 else -1)) in
		match i	with
		| i when i = center - 1                       -> '<'
		| i when i = center                           -> machine.tape.[machine.index]
		| i when i = center + 1                       -> '>'
		| _ when 0 <= tape_pos && tape_pos < tape_len -> machine.tape.[tape_pos]
		| _                                           -> machine.rules.blank
	in
	"[" ^ (String.init window_size symbol_i) ^ "]"

let print_tape ?(window_size = 20) (machine: machine) : unit =
	Printf.printf "%s " (tape_to_str ~window_size machine)

let print_step ?(window_size = 20) (machine: machine) (transition:transition) : unit =
	print_tape ~window_size machine;
	Printf.printf "(%s, %c) -> (%s, %c, %s)\n" machine.state machine.tape.[machine.index] transition.to_state
		transition.write (action_to_str transition.action)

exception Symbol_not_in_transition of char * string

let rec get_transition_single (machine: machine) (transitions: transition list) : transition =
	match transitions with
		| [] -> raise (Symbol_not_in_transition (machine.tape.[machine.index], machine.state))
		| h::t -> if machine.tape.[machine.index] = h.read then h else get_transition_single machine t

exception Transition_not_found of string

let rec get_transition_lst (machine:machine) : transition list =
	match List.assoc_opt machine.state machine.rules.transitions with
		| None -> raise (Transition_not_found machine.state)
		| Some lst -> lst

let get_transition machine = machine |> get_transition_lst |> get_transition_single machine

let check_bounds (transition: transition) (machine: machine) : machine =
	match transition.action with
		| Left when machine.index = 0 -> { machine with index = 1; tape = char_to_string machine.rules.blank ^ machine.tape }
		| Right when machine.index = String.length machine.tape -> { machine with tape = machine.tape ^ char_to_string machine.rules.blank }
		| _ -> machine

let execute_cell (machine : machine) : machine =
	let halt_machine = { machine with state = List.hd machine.rules.finals } in
	try begin
		let transition = get_transition machine in
		print_step ~window_size:50 machine transition;
		{
			machine with
			tape = String.mapi (fun i c -> if i = machine.index then transition.write else c) machine.tape;
			index = machine.index + (action_to_int transition.action);
			state = transition.to_state;
		} |> check_bounds transition
	end with
		| Symbol_not_in_transition (symbol, state) -> print_err "Case %c not handled in transition `%s' in tape %s\n"
			symbol state (tape_to_str machine);
			halt_machine
		| Transition_not_found (state) -> print_err "Transition `%s' not found\n" state;
			halt_machine

let start_machine (input : string) : string =
	let rec go (machine : machine) : string =
		match List.mem machine.state machine.rules.finals with
			| true -> machine.tape
			| false -> execute_cell machine |> go
	in go {
		rules = dicti;
		tape = input;
		index = 0;
		state = dicti.initial;
	}
