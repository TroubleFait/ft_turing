module JSON = struct
  include Parser
end

module CharMap = Rules_parser.CharMap

let char_to_string c = String.make 1 c

exception Symbol_not_in_transition of char * string
exception Transition_not_found of string
exception Endless_loop of int * string

let print_err fmt =
	Printf.printf "%!";
	Printf.eprintf ("\027[31m" ^^ fmt ^^ "\027[0m")

type rules = Rules_parser.rules
type transition = Rules_parser.transition
type action = Rules_parser.action
let action_to_int = Rules_parser.action_to_int
let action_to_str = Rules_parser.action_to_str

type machine = {
	rules: rules;
	tape: string;
	index: int;
	state: string;
	last_change: machine option;
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
	Printf.printf "i: %d (%s, %c) -> (%s, %c, %s)\n" machine.index machine.state machine.tape.[machine.index] transition.to_state
		transition.write (action_to_str transition.action)

(* let rec get_transition_single (machine: machine) (transitions: transition list) : transition = *)
(* 	match transitions with *)
(* 	| [] -> raise (Symbol_not_in_transition (machine.tape.[machine.index], machine.state)) *)
(* 	| h::t -> (* Printf.printf "Here2: %d" machine.index; *) if machine.tape.[machine.index] = h.read then h else get_transition_single machine t *)


(* let rec get_transition_lst (machine:machine) : transition list = *)
(* 	match List.assoc_opt machine.state machine.rules.transitions with *)
(* 	| None -> raise (Transition_not_found machine.state) *)
(* 	| Some lst -> lst *)

(* let get_transition machine = machine |> get_transition_lst |> get_transition_single machine *)

let get_transition machine =
	JSON.StringMap.find machine.state machine.rules.transitions
	|> CharMap.find machine.tape.[machine.index]

let write_cell (transition: transition) (machine: machine) : machine =
		{
			machine with
			tape = begin match machine.last_change with
				| None -> raise (Endless_loop (machine.index, "No last change (Impossible to happen)"))
				| Some last_change -> last_change.tape end;
			index = machine.index + (action_to_int transition.action);
			state = transition.to_state;
		}

let check_bounds (transition: transition) (machine: machine) : machine =
(* 	Printf.printf "Checking loop: %s\n" (match machine.last_change with *)
(* 	| None -> "None" *)
(* 	| Some old -> Printf.sprintf "Some {index=%d; state=%s; tape='%s'}" old.index old.state old.tape); *)
	match transition.action with
	| Left when machine.index = 0 -> begin
		match machine.tape.[0] with
		| c when c = machine.rules.blank && transition.to_state = machine.state -> raise (Endless_loop (0, "Infinite Left"))
		| _ -> { machine with index = 1; tape = char_to_string machine.rules.blank ^ machine.tape } end
	| Right when machine.index >= (String.length machine.tape - 1) -> begin
		match machine.tape.[(String.length machine.tape) - 1] with
		| c when c = machine.rules.blank && transition.to_state = machine.state -> raise (Endless_loop (machine.index, "Infinite Right"))
		| _ -> { machine with tape = machine.tape ^ char_to_string machine.rules.blank } end
	| _ -> machine

let check_loop (transition:transition) (machine: machine) : machine =
	let new_last_change () = Some { machine with
    tape = String.mapi (fun i c -> if i = machine.index then transition.write else c) machine.tape;
    last_change = None }
  in

  match machine.last_change with
  | None -> { machine with last_change = Some machine }
  | Some last_change when last_change.index = machine.index && last_change.state = transition.to_state &&
      last_change.tape = machine.tape -> raise (Endless_loop (machine.index, "State and tape unchanged since last turn"))
  | _ -> if machine.tape.[machine.index] <> transition.write then {
      machine with last_change = new_last_change () } else machine

let execute_cell (machine : machine) : machine =
	let halt_machine = { machine with state = List.hd machine.rules.finals } in
	try begin
		let transition = get_transition machine in
		print_step ~window_size:50 machine transition;
		check_bounds transition machine |> check_loop transition |> write_cell transition
	end with
		| Transition_not_found (state) -> print_err "Transition `%s' not found\n" state;
			halt_machine
		| Symbol_not_in_transition (symbol, state) -> print_err "Case `%c' not handled in transition `%s' in tape %s\n"
			symbol state (tape_to_str machine);
			halt_machine
		| Endless_loop (index, direction) -> print_err "Endless loop detected at index %d; reason %s\n" index direction;
			halt_machine

let start_machine (input : string) (rules:rules) : string =
	let rec go (machine : machine) : string =
		match List.mem machine.state machine.rules.finals with
		| true -> machine.tape
		| false -> execute_cell machine |> go
	in go {
		rules = rules;
		tape = if input = "" then char_to_string rules.blank else input;
		index = 0;
		state = rules.initial;
		last_change = None;
	}
