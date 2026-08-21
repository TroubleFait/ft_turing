type action_t = Left | Right

let action_to_int = function
	| Left -> -1
	| Right -> 1

let action_to_str = function
  | Left -> "LEFT"
  | Right -> "RIGHT"

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

let dicti = {
	name = "unary_sub";
	alphabet= [ "1"; "."; "-"; "=" ];
	blank = ".";
	states = [ "scanright"; "eraseone"; "subone"; "skip"; "HALT" ];
	initial = "scanright";
	finals = [ "HALT" ];
	transitions = [
		("scanright", [
			{ read= "."; to_state= "scanright"; write= "."; action= Right};
			{ read= "1"; to_state= "scanright"; write= "1"; action= Right};
			{ read= "-"; to_state= "scanright"; write= "-"; action= Right};
			{ read= "="; to_state= "eraseone" ; write= "."; action= Left }
		]);
		("eraseone", [
			{ read= "1"; to_state= "subone"; write= "="; action= Left};
			{ read= "-"; to_state= "HALT" ; write= "."; action= Left}
		]);
		("subone", [
			{ read= "1"; to_state= "subone"; write= "1"; action= Left};
			{ read= "-"; to_state= "skip" ; write= "-"; action= Left}
		]);
		("skip", [
			{ read= "."; to_state= "skip" ; write= "."; action= Left};
			{ read= "1"; to_state= "scanright"; write= "."; action= Right}
		])
	]
}

let print_tape (machine: machine_t) : unit =
	Printf.printf "[";
	let rec go str = function
		| i when i >= 20 -> ()
		| i when i = machine.index -> Printf.printf "<%c>" str.[i]; go str (i + 1)
		| i when (i >= String.length str) && (i < 20) -> Printf.printf "%s" machine.dict.blank; go str (i + 1)
		| i -> Printf.printf "%c" str.[i]; go str (i + 1)
	in go machine.tape 0;
	Printf.printf "] "

let print_step (machine: machine_t) (transition:transition_t) : unit =
	print_tape machine;
	Printf.printf "(%s, %c) -> (%s, %c, %s)\n" machine.state machine.tape.[machine.index] transition.to_state
		transition.write.[0] (action_to_str transition.action)

let rec get_transition_single (machine: machine_t) (transitions: transition_t list)  : transition_t =
	match transitions with
		| [] -> failwith ("Case " ^ (String.make 1 machine.tape.[machine.index]) ^ " not handled in transition" ^ machine.state)
		| h::t -> if machine.tape.[machine.index] = h.read.[0] then h else get_transition_single machine t

let rec get_transition_lst (machine:machine_t) : transition_t list =
	match List.assoc_opt machine.state machine.dict.transitions with
		| None -> failwith "Transition not found"
		| Some lst -> lst

let get_transition machine = machine |> get_transition_lst |> get_transition_single machine

let execute_cell (machine : machine_t) : machine_t =
	let transition = get_transition machine in
	print_step machine transition;
	{
		dict = machine.dict;
		tape = String.mapi (fun i c -> if i = machine.index then transition.write.[0] else c) machine.tape;
		index = machine.index + (action_to_int transition.action);
		state = transition.to_state;
	}

let start_machine (input : string) : string =
	let rec go (machine : machine_t) : string =
		match List.mem machine.state machine.dict.finals with
			| true -> machine.tape
			| false -> execute_cell machine |> go
	in go {
		dict = dicti;
		tape = input;
		index = 0;
		state = dicti.initial;
	}
