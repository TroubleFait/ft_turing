module CharHash   = Utils.CharHash
module StringHash = Utils.StringHash

type action = Rules.action


let initial_name : string =
  "initial"

let go_B_name ~(state: char) : string =
  "go_B_" ^ String.of_char state

let put_C_name ~(state: char) : string =
  "put_C_" ^ String.of_char state

let put_E_name ~(state: char) : string =
  "put_E_" ^ String.of_char state

let go_F_name ~(state: char) ~(symbol: char) : string =
  "go_F_" ^ String.of_char state ^ String.of_char symbol

let go_pipe_name ~(state: char) ~(symbol: char) : string =
  "go_|_" ^ String.of_char state ^ String.of_char symbol

let get_state_name ~(state: char) ~(symbol: char) : string =
  "get_state_" ^ String.of_char state ^ String.of_char symbol

let get_read_name ~(state: char) ~(symbol: char) : string =
  "get_read_" ^ String.of_char state ^ String.of_char symbol


let get_to_state_name : string =
  "get_to_state"

let get_write_name ~(to_state: char) : string =
  "get_write_" ^ String.of_char to_state

let get_action_name ~(to_state: char) ~(write: char) : string =
  "get_action_" ^ String.of_char to_state ^ String.of_char write

let go_C_put_name ~(to_state: char) ~(write: char) ~(action: Rules.action) : string =
  "go_C_put_" ^ String.of_char to_state ^ String.of_char write
    ^ match action with
    | Left  -> "L"
    | Right -> "R"

let shift_write_name ~(state: char) ~(symbol: char) : string =
  "shift_write_" ^ String.of_char state ^ String.of_char symbol


let charhash_of_list lst = lst |> List.to_seq |> CharHash.of_seq

let build_transitions_list transition_of_symbol ?(init = []) alphabet =
  String.fold_left ( fun acc c -> (c, transition_of_symbol c) :: acc ) init alphabet
  |> charhash_of_list


let initial ~(states: string) =
  let name = initial_name in
  let transition_of_symbol c = Rules.{
      read = c; write = c; action = Right;
      to_state = go_B_name ~state:c
    }
  in
  name,
  build_transitions_list transition_of_symbol states


let go ~(alphabet: string) ~(to_state: string) ~(action: Rules.action) ~(destination: Rules.transition)
: Rules.state =
  let transition_of_symbol c =
    if c = destination.read then
      destination
    else
      { read = c; to_state; write = c; action }
  in
  build_transitions_list transition_of_symbol alphabet

let get ~(alphabet: string) ~(to_state: string) ~(destination: Rules.transition)
: Rules.state =
  let transition_of_symbol c =
    if c = destination.read then
      destination
    else
      { read = c; to_state; write = c; action = Right }
  in
  build_transitions_list transition_of_symbol alphabet


let go_B ~(alphabet: string) ~(state: char) : string * Rules.state =
  let name = go_B_name ~state in
  let to_state = put_C_name ~state in
  let destination = Rules.{
      read = 'B'; to_state; write = 'B'; action = Right
    }
  in
  name,
  go ~alphabet ~to_state:name ~action:Right ~destination

let put_C ~(tape_alphabet: string) ~(state: char) : string * Rules.state =
  let name = put_C_name ~state in
  let transition_B = 'B', Rules.{
      read = 'B'; write = 'B'; action = Right;
      to_state = shift_write_name ~state ~symbol:'C'
    }
  in
  let transition_E = 'E', Rules.{
      read = 'E'; write = 'C'; action = Right;
      to_state = put_E_name ~state
    }
  in
  let transition_of_symbol symbol = Rules.{
      read = symbol; write = 'C'; action = Left;
      to_state = go_F_name ~state ~symbol
    }
  in
  name,
  build_transitions_list transition_of_symbol ~init:([ transition_B; transition_E ]) tape_alphabet

let put_E ~(blank: char) ~(state: char) : string * Rules.state =
  let name = put_E_name ~state in
  let transition = blank, Rules.{
      read = blank; write = 'E'; action = Left;
      to_state = go_F_name ~state ~symbol:blank
    }
  in
  name,
  charhash_of_list [ transition ]

let go_F ~(alphabet: string) ~(state: char) ~(symbol: char) : string * Rules.state =
  let name = go_F_name ~state ~symbol in
  let destination = Rules.{
      read = 'F'; write = 'F'; action = Right;
      to_state = go_pipe_name ~state ~symbol
    }
  in
  name,
  go ~alphabet ~to_state:name ~action:Left ~destination

let go_pipe ~(alphabet: string) ~(state: char) ~(symbol: char) : string * Rules.state =
  let name = go_pipe_name  ~state ~symbol in
  let destination = Rules.{
      read = '|'; write = '|'; action = Right;
      to_state = get_state_name ~state ~symbol
    }
  in
  name,
  (* no 'F' in alphabet *)
  go ~alphabet ~to_state:name ~action:Right ~destination

let get_state ~(state: char) ~(symbol: char) : string * Rules.state =
  let name = get_state_name ~state ~symbol in
  let alphabet = String.init (int_of_char state - int_of_char 'a' + 1)
    (fun i -> i + int_of_char 'a' |> char_of_int)
  in
  let destination = Rules.{
      read = state; write = state; action = Right;
      to_state = get_read_name ~state ~symbol
    }
  in
  let to_state = go_pipe_name ~state ~symbol in
  name, 
  get ~alphabet ~to_state ~destination

let get_read ~(tape_alphabet: string) ~(state: char) ~(symbol: char) : string * Rules.state =
  let name = get_read_name ~state ~symbol in
  let to_state = go_pipe_name ~state ~symbol in
  let destination = Rules.{
      read = symbol; to_state = get_to_state_name; write = symbol; action = Right;
    }
  in
  name,
  get ~alphabet:tape_alphabet ~to_state ~destination


let get_to_state ~(states: string) =
  let name = get_to_state_name in
  let transition_of_symbol c = Rules.{
      read = c; write = c; action = Right;
      to_state = "get_write_" ^ String.of_char c
    }
  in
  name,
  build_transitions_list transition_of_symbol (states ^ "H")

let get_write ~(writeable_symbols: string) ~(to_state: char) : string * Rules.state =
  let name = get_write_name ~to_state in
  let transition_of_symbol c = Rules.{
      read = c; write = c; action = Right;
      to_state = get_action_name ~to_state ~write:c
    }
  in
  name,
  build_transitions_list transition_of_symbol writeable_symbols

let get_action ~(actions: Rules.action list) ~(to_state: char) ~(write: char) : string * Rules.state =
  let name = get_action_name ~to_state ~write in
  let char_of_action = function
    | Rules.Left  -> 'L'
    | Rules.Right -> 'R'
  in
  let transition_of_action action = let c = char_of_action action in
    c, Rules.{
      read = c; write = c; action = Right;
      to_state = go_C_put_name ~to_state ~write ~action
    }
  in
  name,
  charhash_of_list ( List.map transition_of_action actions )

let go_C_put ~(alphabet: string) ~(to_state: char) ~(write: char) ~(action: Rules.action) : string * Rules.state =
  let name = go_C_put_name ~to_state ~write ~action in
  let destination = Rules.{
      read = 'C'; write = write; action;
      to_state = if to_state = 'H' then "HALT" else put_C_name ~state:to_state
    }
  in
  name,
  go ~alphabet ~to_state:name ~action:Right ~destination


let shift_write ~(tape_alphabet: string) ~(blank: char) ~(state: char) ~(symbol: char) : string * Rules.state =
  let name = shift_write_name ~state ~symbol in
  let transition_go_F = blank, Rules.{
      read = blank; write = symbol; action = Left;
      to_state = go_F_name ~state ~symbol:blank
    }
  in
  let transition_of_symbol c = Rules.{
      read = c; write = symbol; action = Right;
      to_state = put_E_name ~state
    }
  in
  name,
  match symbol with
  | 'E' -> charhash_of_list [ transition_go_F ]
  |  _  -> build_transitions_list transition_of_symbol (tape_alphabet ^ "E")


module CharMap   = Map.Make(Char)
module StringMap = Map.Make(String)

let generator (rules: Rules.rules) : Rules.rules =
  let assoc : char StringHash.t = Encoder.states_assoc rules in
  let states =
    String.init 
      (( StringHash.to_seq_values assoc |> List.of_seq |> List.sort_uniq compare |> List.length ) - 1)
      ( fun i -> (int_of_char 'a') + i |> char_of_int )
  in
  let name = rules.name ^ "_ception" in
  let alphabet =
      "F|RLBECH"
      ^ states
      ^ rules.alphabet
  in
  let transitions =
    let tape_alphabet = rules.alphabet in
    let blank = rules.blank in
    let fold_state init (name: string) =
      let state = StringHash.find assoc name in
      let init_state =
        go_B           ~alphabet             ~state
        :: put_C       ~tape_alphabet        ~state
        :: put_E                      ~blank ~state
        :: shift_write ~tape_alphabet ~blank ~state ~symbol:'C'
        :: init
      in
      let fold_tape_alphabet init symbol =
        go_F           ~alphabet             ~state ~symbol
        :: go_pipe     ~alphabet             ~state ~symbol
        :: get_state                         ~state ~symbol
        :: get_read    ~tape_alphabet        ~state ~symbol
        :: shift_write ~tape_alphabet ~blank ~state ~symbol
        :: init
      in
      tape_alphabet
      |> String.fold_left fold_tape_alphabet init_state
    in
    let add_go_C_put init =
      rules.transitions
      |> StringHash.to_seq_values
      |> Seq.fold_left (fun acc state ->
          state
          |> CharHash.to_seq_values
          |> Seq.fold_left ( fun acc (transition: Rules.transition) ->
            let to_state, write, action =
              StringHash.find assoc transition.to_state, transition.write, transition.action
            in
            go_C_put ~alphabet ~to_state ~write ~action :: acc
          ) acc
        ) init
    in
    let write_map, action_map =
      rules.transitions
      |> StringHash.to_seq_values
      |> Seq.fold_left ( fun acc state ->
          state
          |> CharHash.to_seq_values
          |> Seq.fold_left ( fun acc (transition: Rules.transition) ->
            let char_of_to_state = StringHash.find assoc transition.to_state in
            CharMap.add_to_list (char_of_to_state) transition.write (fst acc),
            StringMap.add_to_list
              (String.of_char char_of_to_state ^ String.of_char transition.write)
              transition.action (snd acc)
          ) acc
        ) (CharMap.empty, StringMap.empty)
    in
    rules.transitions
    |> StringHash.to_seq_keys
    |> Seq.fold_left fold_state [ initial ~states; get_to_state ~states ]
    |> CharMap.fold ( fun to_state writes lst ->
        let writeable_symbols = writes |> List.to_seq |> String.of_seq in
        get_write ~writeable_symbols ~to_state :: lst
      ) write_map
    |> StringMap.fold (fun to_state_write actions lst ->
        get_action ~actions ~to_state:(to_state_write.[0]) ~write:(to_state_write.[1]) :: lst
      ) action_map
    |> add_go_C_put
    |> List.to_seq
    |> StringHash.of_seq
  in
  Rules.{
    name;
    alphabet;
    blank = rules.blank;
    states = "HALT" :: (StringHash.to_seq_keys transitions |> List.of_seq);
    initial = initial_name;
    finals = [ "HALT" ];
    transitions;
  }
