module JSON = struct
  include Parser
end

module CharHash = Utils.CharHash

exception Symbol_not_in_transition of char * string
exception Endless_loop of int * string

type rules = Rules.rules
type transition = Rules.transition
type action = Rules.action
let action_to_int = Rules.action_to_int
let action_to_str = Rules.action_to_str

type machine = {
	rules: rules;
	tape: Tape.t;
	index: int;
	state: string;
	last_change: machine option;
}

let is_new_letter (machine: machine) (transition: transition) : bool =
	Tape.read machine.tape <> transition.write

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
	let before_cursor =
		let rec go i acc = function
		| _ when i <= 0 -> acc
		| [] -> go (i - 1) ((String.of_char machine.rules.blank) ^ acc) []
		| head::tail -> go (i - 1) ((String.of_char head) ^ acc) tail
		in
		go (center - 1)  "" @@ List.tl machine.tape.left
	in
	let after_cursor =
		let rec go i acc = function
		| _ when i <= 0 -> acc
		| [] -> go (i - 1) (acc ^ (String.of_char machine.rules.blank)) []
		| head::tail -> go (i - 1) (acc ^ (String.of_char head)) tail
		in
		go (window_size - center - 2) "" machine.tape.right
	in
	"["
	^ before_cursor
	^ cursor_colour machine transition (String.of_char @@ Tape.read machine.tape)
	^ after_cursor
	^ "]"

let print_step ?(window_size = 20) (machine: machine) (transition:transition) : unit =
	Printf.printf "%s " (tape_to_window_str ~window_size machine transition);
	Printf.printf "(%s, %c) -> (%s, %s, %s)\n%!" machine.state (Tape.read machine.tape)
		( if machine.state <> transition.to_state then new_state_color transition.to_state else transition.to_state )
		( if is_new_letter machine transition then new_letter_colour (String.of_char transition.write) else (String.of_char transition.write) )
		(action_to_str transition.action)

let get_transition (machine: machine): transition =
	try begin
		CharHash.find
			(Utils.StringHash.find machine.rules.transitions machine.state)
			(Tape.read machine.tape)
	end with Not_found -> raise @@ Symbol_not_in_transition (Tape.read machine.tape, machine.state)

let write_cell (transition: transition) (machine: machine) : machine =
(* 	Printf.printf "Checking loop: %s\n" (match machine.last_change with *)
(* 	| None -> "None" *)
(* 	| Some old -> Printf.sprintf "tape: %s, old {index=%d; state=%s; tape='%s'}" machine.tape old.index old.state old.tape); *)
		{
			machine with
	    tape = begin match is_new_letter machine transition with
				| true  -> Tape.write transition.write machine.tape
				| false -> machine.tape
				end
				|> Tape.move machine.rules.blank transition.action;
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
	| Left when Tape.is_begin machine.tape -> begin
		match Tape.read machine.tape with
		| c when c = machine.rules.blank && transition.to_state = machine.state -> raise @@ Endless_loop (0, "Infinite Left")
		| _ -> { machine with
				index = 1;
				last_change = Some {
						old with index = old.index + 1;
						tape = old.tape
							|> Tape.move machine.rules.blank Left
							|> Tape.move machine.rules.blank Right
					}
			} end
	| Right when Tape.is_end machine.tape -> begin
		match Tape.read machine.tape with
		| c when c = machine.rules.blank && transition.to_state = machine.state -> raise @@ Endless_loop (machine.index, "Infinite Right")
		| _ -> { machine with
				last_change = Some {
						old with
						tape = old.tape
							|> Tape.move machine.rules.blank Right
							|> Tape.move machine.rules.blank Left
					}
			} end
	| _ -> machine

let check_loop (transition:transition) (machine: machine) : machine =
	let new_last_change () = Some { machine with
    (* tape = String.mapi (fun i c -> if i = machine.index then transition.write else c) machine.tape; *)
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
			symbol state (Tape.to_string machine.tape);
			halt_machine
		| Endless_loop (index, direction) -> Utils.print_err "Endless loop detected at index %d; reason %s\n" index direction;
			halt_machine
		| Tape.Misplaced_cursor msg -> Utils.print_err "%s\n" msg;
			halt_machine

let start_machine (input : string) (rules:rules) : Tape.t * char =
	let rec go (machine : machine) : Tape.t * char =
		match Utils.StringHash.find_opt machine.rules.transitions machine.state with
		| Some _ -> execute_cell machine |> go
		| None -> machine.tape, machine.rules.blank (*is a final state*)
	in go {
		rules = rules;
		tape = if input = "" then Tape.of_string @@ String.of_char rules.blank else Tape.of_string input;
		index = 0;
		state = rules.initial;
		last_change = None;
	}
