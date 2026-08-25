module JSON = struct
	include Parser
end

module CharMap = Map.Make(Char)

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
	transitions: (transition CharMap.t) JSON.StringMap.t;
}

let string_of_JSON_value (empty_error:string) (invalid_type_error:string) = function
	| JSON.String "" -> failwith empty_error
	| JSON.String s -> s
	| _ -> failwith invalid_type_error

let char_of_JSON_value ?(symbol_error = "invalid symbol") ?(invalid_type_error = "invalid symbol type") = function
	| JSON.String s when String.length s = 1 -> s.[0]
	| JSON.String s -> failwith (symbol_error ^ ":`" ^ s ^ "'")
	| _ -> failwith invalid_type_error

let action_of_JSON_value = function
	| JSON.String "LEFT"  -> Left
	| JSON.String "RIGHT" -> Right
	| JSON.String s -> failwith ("Not LEFT or RIGHT: `" ^ s ^ "'")
	| _ -> failwith "action is not a string"

let check_binding_keys (to_compare: string list) (bindings: string list) =
	to_compare
	|> List.sort compare
	|> List.equal (=) bindings

let create_name = string_of_JSON_value "empty name" "invalid name type"

let create_alphabet = function
	| JSON.Array a -> String.init (Array.length a) (fun i -> a.(i)
			|> char_of_JSON_value)
	| _ -> failwith "invalid alphabet type"

let create_blank = char_of_JSON_value ~invalid_type_error:"invalid blank type"

let create_states = function
	| JSON.Array a -> Array.to_list a
		|> List.map (string_of_JSON_value "empty state name" "invalid state type")
	| _ -> failwith "invalid states type"

let create_initial = string_of_JSON_value "empty initial state" "invalid initial type"

let create_finals = function
	| JSON.Array a -> Array.to_list a
		|> List.map (string_of_JSON_value "empty final state name" "invalid final state type")
	| _ -> failwith "invalid finals type"

let create_transitions v =
	let transition_of_JSON_object acc = function
		| JSON.Object obj
			when JSON.StringMap.bindings obj |> List.split |> fst
			|> check_binding_keys ["read"; "to_state"; "write"; "action"]
			-> let read_key = char_of_JSON_value @@ JSON.StringMap.find "read" obj in
				CharMap.add read_key {
					read     = read_key;
					to_state = string_of_JSON_value "empty to_state" "invalid to_state"
																					@@ JSON.StringMap.find "to_state" obj;
					write    = char_of_JSON_value   @@ JSON.StringMap.find "write"    obj;
					action   = action_of_JSON_value @@ JSON.StringMap.find "action"   obj;
				} acc
		| JSON.Object _ -> failwith "invalid transition structure"
		| _ -> failwith "invalid transition type"
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
	| JSON.Object _ -> failwith "invalid object structure"
	| _ -> failwith "Expected an object"

let validate_alphabet (alphabet: string) =
	String.iteri (fun i c ->
		if String.rindex alphabet c <> i then
			failwith "duplicate symbol in alphabet"
	) alphabet

let validate_char ?(fail_msg = "unknown symbol") (alphabet: string) (char: char) =
	String.index alphabet char |> ignore

let validate_state (fail_msg: string) (states: string list) (state: string) =
	match List.mem state states with
	| true  -> ()
	| false -> failwith fail_msg

let validate_states (finals: string list) (transitions: string list) (states: string list) =
	match List.equal (=) (List.sort compare states) (List.sort compare (transitions @ finals)) with
	| true  -> ()
	| false -> failwith "mismatch between states, transitions and finals"

let validate_finals (states: string list) (finals: string list) =
	List.iter (validate_state "unknown final" states) finals

let validate_transition (alphabet: string) (states: string list) (transition: transition CharMap.t) =
	CharMap.bindings transition |> List.split |> snd
	|> List.iter (fun v ->
		validate_char alphabet v.read;
		validate_state "unknown to_state" states v.to_state;
		validate_char alphabet v.write
	)

let validate_transitions (alphabet: string) (states: string list) (transitions: (transition CharMap.t) JSON.StringMap.t) =
	JSON.StringMap.bindings transitions |> List.split |> snd
	|> List.iter (validate_transition alphabet states)

let validate_rules (rules: rules) : rules =
	validate_alphabet    rules.alphabet;
  validate_char        rules.alphabet rules.blank;
  validate_states      rules.finals   (JSON.StringMap.bindings rules.transitions |> List.split |> fst) rules.states;
  validate_state       "unknown initial" rules.states   rules.initial;
  validate_finals      rules.states   rules.finals;
  validate_transitions rules.alphabet rules.states rules.transitions;
  rules
