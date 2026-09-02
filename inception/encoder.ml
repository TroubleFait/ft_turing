module StringHash = Utils.StringHash

let validate_puctuation (alphabet: string) : unit =
  let punctuation = "F|XCH" in
  match Utils.find_first_not_of punctuation alphabet with
  | Some 0 -> ()
  | _ -> failwith "alphabet contains punctuation"

let validate_size (transitions: Rules.state StringHash.t) : unit =
  if StringHash.length transitions > 26 then
    failwith "too many transitions in the machine"

module StringMap = Map.Make(String)

let states_assoc (rules: Rules.rules) : string StringHash.t =
  validate_puctuation rules.alphabet;
  validate_size rules.transitions;
  let encode_statei map i state =
    StringMap.add state (
      (int_of_char 'a') + i |> char_of_int
    ) map
  in
  let encode_final map final =
    StringMap.add final 'H' map
  in
  let transition_states_map =
    rules.transitions
    |> Utils.StringHash.to_seq_keys
    |> Seq.fold_lefti encode_statei StringMap.empty
  in
  rules.finals
  |> List.fold_left encode_final transition_states_map
  |> StringMap.to_seq
  |> StringHash.of_seq

let encode_transitions (assoc: string StringHash.t) (rules: Rules.rules) : string =
  let encode_action = function
  | Rules.Left  -> "L"
  | Rules.Right -> "R"
  in
  let encode_transition acc (transition_state, transition) =
    acc
    ^ "|"
    ^ StringHash.find assoc transition_state
    ^ String.of_char transition.read
    ^ StringHash.find assoc transition.to_state
    ^ String.of_char transition.write
    ^ encode_action transition.action
  in
  rules.transitions
  |> StringHash.to_seq
  |> Seq.fold_left encode_transition ""

let encode_machine (assoc: string StringHash.t) (rules: Rules.rules) (tape: string) : string =
  StringHash.find assoc rules.initial ^ "F" ^ encode_transitions assoc rules ^ "X" ^ tape
