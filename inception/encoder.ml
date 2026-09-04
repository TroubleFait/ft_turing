module CharHash   = Utils.CharHash
module StringHash = Utils.StringHash

let validate_puctuation (alphabet: string) : unit =
  if Utils.find_first_not_of "F|HBEC" alphabet <> Some 0 then
    failwith "alphabet contains punctuation"

let validate_size (transitions: Rules.state StringHash.t) : unit =
  if StringHash.length transitions > 26 then
    failwith "too many transitions in the machine"

module StringMap = Map.Make(String)

let states_assoc (rules: Rules.rules) : char StringHash.t =
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

let encode_transitions (assoc: char StringHash.t) (rules: Rules.rules) : string =
  let encode_transition state acc (transition: Rules.transition) =
    let prefix = "|" ^ ( StringHash.find assoc state |> String.of_char ) in
    acc
    ^ prefix
    ^ String.of_char transition.read
    ^ ( StringHash.find assoc transition.to_state |> String.of_char)
    ^ String.of_char transition.write
    ^ match transition.action with
    | Rules.Left  -> "L"
    | Rules.Right -> "R"
  in
  let encode_state acc (state, transitions: string * Rules.state) =
   transitions
    |> CharHash.to_seq_values
    |> Seq.fold_left (fun acc transition -> encode_transition state acc transition) ""
    |> String.cat acc
  in
  rules.transitions
  |> StringHash.to_seq
  |> Seq.fold_left encode_state ""

let encode_machine (rules: Rules.rules) (tape: string) : string =
  let assoc = states_assoc rules in
  let encoded_tape =
    ( StringHash.find assoc rules.initial |> String.of_char )
    ^ "F"
    ^ encode_transitions assoc rules
    ^ "B" ^ tape ^ "E"
  in
  Printf.printf "%s encoded as a tape:\n" rules.name;
  Printf.printf "%s\n\n" encoded_tape;
  encoded_tape
