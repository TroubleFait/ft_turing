module JSON = struct
	include Parser
end

module CharHash   = Utils.CharHash
module StringHash = Utils.StringHash

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

type state = transition CharHash.t

type rules = {
	name: string;
	alphabet: string;
	blank: char;
	states: string list;
	initial: string;
	finals: string list;
	transitions: state StringHash.t;
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

let check_keys (tbl: JSON.value_t StringHash.t) (to_compare: string list) =
	let keys =
		tbl |> StringHash.to_seq_keys |> List.of_seq |> List.sort compare
	in
	to_compare
	|> List.sort compare
	|> List.equal (=) keys

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

module CharMap = Map.Make(Char)

let create_transitions v =
	let transition_of_JSON_object acc = function
		| JSON.Object obj
			when ["read"; "to_state"; "write"; "action"]
			|> check_keys obj
			-> let read_key = char_of_JSON_value ~prefix:"Read: " @@ StringHash.find obj "read" in
				CharMap.add read_key {
					read     = read_key;
					to_state = string_of_JSON_value ~prefix:"to_state: "
																					                @@ StringHash.find obj "to_state";
					write    = char_of_JSON_value ~prefix:"Write: " @@ StringHash.find obj "write";
					action   = action_of_JSON_value                 @@ StringHash.find obj "action";
				} acc
		| JSON.Object _ -> failwith "invalid transition structure"
		| _ -> raise Invalid_struct
	in
	let charhash_of_array (key, value: StringHash.key * JSON.value_t) =
		match value with
		| JSON.Array a -> key, Array.fold_left transition_of_JSON_object CharMap.empty a
			|> CharMap.to_seq
			|> CharHash.of_seq
		| _ -> failwith ""
	in
	match v with
	| JSON.Object obj -> obj
		|> StringHash.to_seq
		|> List.of_seq
		|> List.map charhash_of_array
		|> List.to_seq
		|> StringHash.of_seq
	| _ -> failwith "invalid transitions type"


let create_rules obj =
	{
		name        = create_name        @@ StringHash.find obj "name";
		alphabet    = create_alphabet    @@ StringHash.find obj "alphabet";
		blank       = create_blank       @@ StringHash.find obj "blank";
		states      = create_states      @@ StringHash.find obj "states";
		initial     = create_initial     @@ StringHash.find obj "initial";
		finals      = create_finals      @@ StringHash.find obj "finals";
		transitions = create_transitions @@ StringHash.find obj "transitions";
	}

let parse (json: JSON.value_t) : rules =
	match json with
	| JSON.Object obj
		when ["name"; "alphabet"; "blank"; "states"; "initial"; "finals"; "transitions"]
		|> check_keys obj 
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
	| Not_found -> failwith @@ prefix ^ "`" ^ (String.of_char char) ^ "' " ^ fail_msg

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

let validate_transition (alphabet: string) (states: string list) (transition: transition CharHash.t) =
	match CharHash.to_seq_values transition |> List.of_seq with
	| [] -> failwith "Empty transition"
	| lst -> List.iter (fun v ->
			validate_char   ~prefix:"read: " alphabet v.read;
			validate_state  ~prefix:"to_state: " states v.to_state;
			validate_char   ~prefix:"write: " alphabet v.write;
		) lst

let validate_transitions (alphabet: string) (states: string list) (transitions: (transition CharHash.t) StringHash.t) =
	match StringHash.to_seq_values transitions |> List.of_seq with
	| [] -> failwith "transitions is empty"
	| lst -> List.iter (validate_transition alphabet states) lst

let validate (rules: rules) : rules =
	validate_alphabet    rules.alphabet;
  validate_char        ~fail_msg:"blank symbol is not in alphabet" rules.alphabet rules.blank;
  validate_states      rules.finals   (StringHash.to_seq_keys rules.transitions |> List.of_seq) rules.states;
  validate_state       ~prefix:"Initial: " rules.states   rules.initial;
  validate_finals      rules.states   rules.finals;
  validate_transitions rules.alphabet rules.states rules.transitions;
  rules

let validate_input (tape: string) (rules: rules) : rules =
(* 	match String.index_opt tape rules.blank with *)
(* 	| Some _ -> failwith @@ "Blank char: `" ^ (String.of_char rules.blank) ^ "' is in the input" *)
(* 	| None -> String.iter (validate_char ~fail_msg:("a symbol in input is not in alphabet") rules.alphabet) tape; rules *)
String.iter (validate_char ~fail_msg:("a symbol in input is not in alphabet") rules.alphabet) tape; rules

type halt_reached = Found | Already_explored of string list

let is_HALT_reachable (rules: rules) : rules =
	let transition_is_halt (finals: string list) = function
		| elem when List.mem elem.to_state finals -> true
		| _ -> false
	in
	let rec go = function
		| Found -> Found
		| Already_explored [] -> failwith "empty string list in is_HALT_reachable.go"
		| Already_explored (current :: tail) when List.mem current tail -> Already_explored (tail)
		| Already_explored (current :: tail) ->
		let state = StringHash.find rules.transitions current in
		(* match CharHash.exists (transition_is_halt rules.finals) state with *)
		match CharHash.to_seq_values state
			|> Seq.exists (transition_is_halt rules.finals)
		with
		| true -> Found
		| false -> CharHash.fold (
			fun key transition explored ->
				match explored with
				| Found -> Found
				| Already_explored lst -> go @@ Already_explored (transition.to_state :: lst)
			) state @@ Already_explored (current :: tail)
	in
	match go @@ Already_explored [rules.initial] with
	| Found -> rules
	| _ -> failwith "No final state reachable"
