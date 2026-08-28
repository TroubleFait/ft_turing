module JSON = struct
	include Parser
end

module CharMap = Map.Make(Char)

let print_invalid_struct () = Printf.printf "Invalid object structure; Expected {
	name: string;
	alphabet: string list;
	blank: string;
	states: string list;
	initial: string;
	finals: string list;
	transitions: {
		name: [{
			read: string;
			to_state: string;
			write: string;
			action: string;
		}]
	}
}\n"

exception Invalid_struct

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

type state = transition CharMap.t

type rules = {
	name: string;
	alphabet: string;
	blank: char;
	states: string list;
	initial: string;
	finals: string list;
	transitions: state JSON.StringMap.t;
}

let string_of_JSON_value ?(prefix = "") ?(empty_error = "empty") ?(invalid_type_error = "not a string") = function
	| JSON.String "" -> failwith @@ prefix ^ empty_error
	| JSON.String s -> s
	| _ -> failwith @@ prefix ^ invalid_type_error

let char_of_JSON_value ?(prefix = "") ?(symbol_error = "not a char") ?(invalid_type_error = "not a string") = function
	| JSON.String s when String.length s = 1 -> s.[0]
	| JSON.String s -> failwith @@ prefix ^ symbol_error ^ ":`" ^ s ^ "'"
	| _ -> failwith @@ prefix ^ invalid_type_error

let action_of_JSON_value = function
	| JSON.String "LEFT"  -> Left
	| JSON.String "RIGHT" -> Right
	| JSON.String s -> failwith ("action: Not LEFT or RIGHT: `" ^ s ^ "'")
	| _ -> failwith "action: not a string"

let check_binding_keys (to_compare: string list) (bindings: string list) =
(* 	Printf.printf "to_compare keys:\t %s\n" (to_compare |> List.sort compare |> String.concat ", "); *)
(* 	Printf.printf "checking binding keys:\t %s\n" (String.concat ", " bindings); *)
	to_compare
	|> List.sort compare
	|> List.equal (=) bindings

let create_name = string_of_JSON_value ~prefix:"Name: "

let create_alphabet = function
 	| JSON.Array a when Array.length a <= 0 -> failwith "Empty alphabet"
	| JSON.Array a -> String.init (Array.length a) (fun i -> a.(i)
			|> char_of_JSON_value ~prefix:"Alphabet: ")
	| _ -> failwith "invalid alphabet type"

let create_blank = char_of_JSON_value ~prefix:"Blank: "

let create_states = function
 	| JSON.Array a when Array.length a <= 0 -> failwith "Empty list of states"
	| JSON.Array a -> Array.to_list a
		|> List.map (string_of_JSON_value ~prefix:"States: one is ")
	| _ -> failwith "invalid states type"

let create_initial = string_of_JSON_value ~prefix:"Initial: "

let create_finals = function
 	| JSON.Array a when Array.length a <= 0 -> failwith "Empty list of finals"
	| JSON.Array a   -> Array.to_list a
		|> List.map (string_of_JSON_value ~prefix:"Finals: one is ")
	| _ -> failwith "invalid finals type"

let create_transitions v =
	let transition_of_JSON_object acc = function
		| JSON.Object obj
			when JSON.StringMap.bindings obj |> List.split |> fst
			|> check_binding_keys ["read"; "to_state"; "write"; "action"]
			-> let read_key = char_of_JSON_value ~prefix:"Read: " @@ JSON.StringMap.find "read" obj in
				CharMap.add read_key {
					read     = read_key;
					to_state = string_of_JSON_value ~prefix:"to_state: "
																					                @@ JSON.StringMap.find "to_state" obj;
					write    = char_of_JSON_value ~prefix:"Write: " @@ JSON.StringMap.find "write"    obj;
					action   = action_of_JSON_value                 @@ JSON.StringMap.find "action"   obj;
				} acc
		| JSON.Object _ -> failwith "invalid transition structure"
		| _ -> raise Invalid_struct
	in
	let charmap_of_array = function
		| JSON.Array a -> Array.fold_left transition_of_JSON_object CharMap.empty a
		| _ -> failwith ""
	in
	match v with
	| JSON.Object obj -> JSON.StringMap.map charmap_of_array obj
	| _ -> failwith "invalid transitions type"


let create_rules obj =
	{
		name        = create_name        @@ JSON.StringMap.find "name"        obj;
		alphabet    = create_alphabet    @@ JSON.StringMap.find "alphabet"    obj;
		blank       = create_blank       @@ JSON.StringMap.find "blank"       obj;
		states      = create_states      @@ JSON.StringMap.find "states"      obj;
		initial     = create_initial     @@ JSON.StringMap.find "initial"     obj;
		finals      = create_finals      @@ JSON.StringMap.find "finals"      obj;
		transitions = create_transitions @@ JSON.StringMap.find "transitions" obj;
	}

let parse_rules (json: JSON.value_t) : rules =
	match json with
	| JSON.Object obj
		when JSON.StringMap.bindings obj |> List.split |> fst
		|> check_binding_keys ["name"; "alphabet"; "blank"; "states"; "initial"; "finals"; "transitions"]
		-> create_rules obj
	| JSON.Object _ -> raise Invalid_struct
	| _ -> raise Invalid_struct

let validate_alphabet (alphabet: string) =
	String.iteri (fun i c ->
		if String.rindex alphabet c <> i then
			failwith "duplicate symbol in alphabet"
	) alphabet

let validate_char ?(prefix = "") ?(fail_msg = "is not in alphabet") (alphabet: string) (char: char) =
	try
		String.index alphabet char |> ignore
	with
	| Not_found -> failwith @@ prefix ^ "`" ^ (Utils.char_to_string char) ^ "' " ^ fail_msg

let validate_state ?(prefix = "") ?(fail_msg = "unknown") (states: string list) (state: string) =
	match List.mem state states with
	| true  -> ()
	| false -> failwith @@ prefix ^ fail_msg ^ ": \"" ^ state ^ "\""

let validate_states (finals: string list) (transitions: string list) (states: string list) =
	match List.equal (=) (List.sort compare states) (List.sort compare (transitions @ finals)) with
	| true  -> ()
	| false -> failwith "mismatch between states, transitions and finals"

let validate_finals (states: string list) (finals: string list) =
	List.iter (validate_state ~prefix:"Finals: " states) finals

let validate_transition (alphabet: string) (states: string list) (transition: transition CharMap.t) =
	match CharMap.bindings transition |> List.split |> snd with
	| [] -> failwith "Empty transition"
	| lst -> List.iter (fun v ->
			validate_char   ~prefix:"read: " alphabet v.read;
			validate_state  ~prefix:"to_state: " states v.to_state;
			validate_char   ~prefix:"write: " alphabet v.write;
		) lst

let validate_transitions (alphabet: string) (states: string list) (transitions: (transition CharMap.t) JSON.StringMap.t) =
	match JSON.StringMap.bindings transitions |> List.split |> snd with
	| [] -> failwith "transitions is empty"
	| lst -> List.iter (validate_transition alphabet states) lst

let validate_rules (rules: rules) : rules =
	validate_alphabet    rules.alphabet;
  validate_char        ~fail_msg:"blank symbol is not in alphabet" rules.alphabet rules.blank;
  validate_states      rules.finals   (JSON.StringMap.bindings rules.transitions |> List.split |> fst) rules.states;
  validate_state       ~prefix:"Initial: " rules.states   rules.initial;
  validate_finals      rules.states   rules.finals;
  validate_transitions rules.alphabet rules.states rules.transitions;
  rules

let validate_input (tape: string) (rules: rules) : rules =
(* 	match String.index_opt tape rules.blank with *)
(* 	| Some _ -> failwith @@ "Blank char: `" ^ (Utils.char_to_string rules.blank) ^ "' is in the input" *)
(* 	| None -> String.iter (validate_char ~fail_msg:("a symbol in input is not in alphabet") rules.alphabet) tape; rules *)
String.iter (validate_char ~fail_msg:("a symbol in input is not in alphabet") rules.alphabet) tape; rules

type halt_reached = Found | Already_explored of string list

let is_HALT_reachable (rules: rules) : rules =
	let transition_is_halt (finals: string list) key = function
		| elem when List.mem elem.to_state finals -> true
		| _ -> false
	in
	let rec go = function
		| Found -> Found
		| Already_explored [] -> failwith "empty string list in is_HALT_reachable.go"
		| Already_explored (current :: tail) when List.mem current tail -> Already_explored (tail)
		| Already_explored (current :: tail) ->
		let state = JSON.StringMap.find current rules.transitions in
		match CharMap.exists (transition_is_halt rules.finals) state with
		| true -> Found
		| false -> CharMap.fold (
			fun key transition explored ->
				match explored with
				| Found -> Found
				| Already_explored lst -> go @@ Already_explored (transition.to_state :: lst)
			) state @@ Already_explored (current :: tail)
	in
	match go @@ Already_explored [rules.initial] with
	| Found -> rules
	| _ -> failwith "No final state reachable"
