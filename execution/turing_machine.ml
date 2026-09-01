module JSON = struct
  include Parser
end

module CharHash = Rules_parser.CharHash

exception Symbol_not_in_transition of char * string
exception Endless_loop of int * string

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

let is_new_letter (machine: machine) (transition: transition) : bool =
	machine.tape.[machine.index] <> transition.write

let is_new_state (machine: machine) (transition: transition) : bool =
	machine.state <> transition.to_state

let new_letter_colour str =
	"\027[0;38;2;166;104;227m\027[1m" ^ str ^ "\027[0m"

let new_state_color str =
	"\027[0;38;2;245;204;22m\027[1m" ^ str ^ "\027[0m"

let cursor_colour str =
	"\027[0;38;2;69;196;196m\027[1m" ^ str ^ "\027[0m"

let cursor_colour (machine: machine) (transition: transition) str =
	begin match transition.action with
		| Left  -> "<" ^ str ^ ")"
		| Right -> "(" ^ str ^ ">"
	end |>
	function
		| s when is_new_letter machine transition -> new_letter_colour s
		| s when is_new_state machine transition -> new_state_color s
		| s -> cursor_colour s

let tape_to_window_str ?(window_size = 20) (machine: machine) (transition: transition) : string =
	let center = (window_size - 1) / 2 in
	let tape_len = String.length machine.tape in
	let symbol_i i =
		let tape_pos = machine.index + i - (center - (if i < center - 1 then 1 else -1)) in
		match i	with
		| _ when 0 <= tape_pos && tape_pos < tape_len -> machine.tape.[tape_pos]
		| _                                           -> machine.rules.blank
	in
	"["
	^ (String.init (center - 1) symbol_i)
	^ cursor_colour machine transition (Utils.char_to_string machine.tape.[machine.index])
	^ (String.init (window_size - center - 2) (fun i -> symbol_i (i + center + 2)))
	^ "]"

let print_tape ?(window_size = 20) (machine: machine) (transition: transition) : unit =
	Printf.printf "%s " (tape_to_window_str ~window_size machine transition)

let print_step ?(window_size = 20) (machine: machine) (transition:transition) : unit =
	print_tape ~window_size machine transition;
	Printf.printf "(%s, %c) -> (%s, %s, %s)\n%!" machine.state machine.tape.[machine.index]
		( if machine.state <> transition.to_state then new_state_color transition.to_state else transition.to_state )
		( if is_new_letter machine transition then new_letter_colour (Utils.char_to_string transition.write) else (Utils.char_to_string transition.write) )
		(action_to_str transition.action)

let get_transition (machine: machine): transition =
	try begin
		JSON.StringHash.find machine.state machine.rules.transitions
		|> CharHash.find machine.tape.[machine.index]
	end with Not_found -> raise @@ Symbol_not_in_transition (machine.tape.[machine.index], machine.state)

let write_cell (transition: transition) (machine: machine) : machine =
(* 	Printf.printf "Checking loop: %s\n" (match machine.last_change with *)
(* 	| None -> "None" *)
(* 	| Some old -> Printf.sprintf "tape: %s, old {index=%d; state=%s; tape='%s'}" machine.tape old.index old.state old.tape); *)
		{
			machine with
 	    tape = begin match is_new_letter machine transition with
 	     | true -> String.mapi (fun i c -> if i = machine.index then transition.write else c) machine.tape
 	     | false -> machine.tape end;
			index = machine.index + (action_to_int transition.action);
			state = transition.to_state;
		}

let check_bounds (transition: transition) (machine: machine) : machine =
(* 	Printf.printf "%s\n" (match machine.last_change with *)
(* 	| None -> "None" *)
(* 	| Some old -> Printf.sprintf "old {index=%d; state=%s; tape='%s'}" old.index old.state old.tape); *)
	match machine.last_change with
	| None -> raise @@ Endless_loop (machine.index, "(Impossible to happen)")
 	| Some old ->
	match transition.action with
	| Left when machine.index = 0 -> begin
		match machine.tape.[0] with
		| c when c = machine.rules.blank && transition.to_state = machine.state -> raise @@ Endless_loop (0, "Infinite Left")
		| _ -> { machine with
				index = 1; tape = Utils.char_to_string machine.rules.blank ^ machine.tape;
				 last_change = Some { old with index = old.index + 1; tape = Utils.char_to_string machine.rules.blank ^ old.tape }
			} end
	| Right when machine.index >= (String.length machine.tape - 1) -> begin
		match machine.tape.[(String.length machine.tape) - 1] with
		| c when c = machine.rules.blank && transition.to_state = machine.state -> raise @@ Endless_loop (machine.index, "Infinite Right")
		| _ -> { machine with
				tape = machine.tape ^ Utils.char_to_string machine.rules.blank;
				last_change = Some { old with tape = old.tape ^ Utils.char_to_string machine.rules.blank }
			} end
	| _ -> machine

let check_loop (transition:transition) (machine: machine) : machine =
	let new_last_change () = Some { machine with
(*    tape = String.mapi (fun i c -> if i = machine.index then transition.write else c) machine.tape; *)
    last_change = None }
  in
  match machine.last_change with
  | None -> { machine with last_change = new_last_change () }
  | Some last_change when
    last_change.index = machine.index && last_change.state = machine.state && last_change.tape = machine.tape ->
      raise (Endless_loop (machine.index, "State and tape unchanged since last turn"))
  | _ -> if is_new_letter machine transition then {
      machine with last_change = new_last_change () } else machine

let execute_cell (machine : machine) : machine =
	let halt_machine = { machine with state = List.hd machine.rules.finals } in
	try begin
		let transition = get_transition machine in
		print_step ~window_size:60 machine transition;
		machine
		|> check_loop transition
		|> check_bounds transition
		|> write_cell transition
	end with
		| Symbol_not_in_transition (symbol, state) -> Utils.print_err "Case `%c' not handled in transition `%s' in tape %s\n"
			symbol state machine.tape;
			halt_machine
		| Endless_loop (index, direction) -> Utils.print_err "Endless loop detected at index %d; reason %s\n" index direction;
			halt_machine

let start_machine (input : string) (rules:rules) : string =
	let rec go (machine : machine) : string =
		match JSON.StringHash.find_opt machine.state machine.rules.transitions with
		| Some _ -> execute_cell machine |> go
		| None -> machine.tape (*is a final state*)
	in go {
		rules = rules;
		tape = if input = "" then Utils.char_to_string rules.blank else input;
		index = 0;
		state = rules.initial;
		last_change = None;
	}
