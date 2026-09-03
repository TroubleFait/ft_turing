module CharHash   = Utils.CharHash
module StringHash = Utils.StringHash

type action = Rules.action

let put_C_name ~(state: char) : string =
  "put_C_" ^ String.of_char state

let put_E_name ~(state: char) : string =
  "go_F_put_E_" ^ String.of_char state

let go_F_name ~(state: char) ~(symbol: char) : string =
  "go_F_" ^ String.of_char state ^ String.of_char symbol

let go_pipe_name ~(state: char) ~(symbol: char) : string =
  "go_|_" ^ String.of_char state ^ String.of_char symbol

let get_state_name ~(state: char) ~(symbol: char) : string =
  "get_state_" ^ String.of_char state ^ String.of_char symbol

let get_read_name ~(state: char) ~(symbol: char) : string =
  "get_read_" ^ String.of_char state ^ String.of_char symbol

let get_write_name ~(state: char) : string =
  "get_write_" ^ String.of_char state

let get_action_name ~(state: char) ~(symbol: char) : string =
  "get_action_" ^ String.of_char state ^ String.of_char symbol

let go_C_put_name ~(state: char) ~(symbol: char) ~(action: Rules.action) : string =
  "go_C_put_" ^ String.of_char state ^ String.of_char symbol ^ Rules.action_to_str action

let shift_write_name ~(state: char) ~(write: char) : string =
  "shift_write_" ^ String.of_char state ^ String.of_char write


let charhash_of_list lst = lst |> List.to_seq |> CharHash.of_seq

let build_transitions_list transition_of_symbol ?(init = []) alphabet =
  String.fold_left ( fun acc c -> (c, transition_of_symbol c) :: acc ) init alphabet
  |> charhash_of_list

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

let put_C ~(tape_alphabet: string) ~(state: char) : string * Rules.state =
  let name = put_C_name ~state in
  let transition_B = 'B', Rules.{
      read = 'B'; write = 'B'; action = Right;
      to_state = shift_write_name ~state ~write:'C'
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

let go_F ~(alphabet: string) ~state_symbol:(state, symbol: char * char) : string * Rules.state =
  let name = go_F_name ~state ~symbol in
  let destination = Rules.{
      read = 'F'; write = 'F'; action = Right;
      to_state = go_pipe_name ~state ~symbol
    }
  in
  name,
  go ~alphabet ~to_state:name ~action:Left ~destination

let go_pipe ~(alphabet: string) ~state_symbol:(state, symbol: char * char) : string * Rules.state =
  let name = go_pipe_name  ~state ~symbol in
  let destination = Rules.{
      read = '|'; write = '|'; action = Right;
      to_state = get_state_name ~state ~symbol
    }
  in
  name,
  (* no 'F' in alphabet *)
  go ~alphabet ~to_state:name ~action:Right ~destination

let get_state ~state_symbol:(state, symbol: char * char) : string * Rules.state =
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

let get_read ~(tape_alphabet: string) ~state_symbol:(state, symbol: char * char) : string * Rules.state =
  let name = get_read_name ~state ~symbol in
  let to_state = go_pipe_name ~state ~symbol in
  let destination = Rules.{
      read = symbol; to_state = "get_to_state"; write = symbol; action = Right;
    }
  in
  name,
  get ~alphabet:tape_alphabet ~to_state ~destination

let get_write ~(tape_alphabet: string) ~(state: char) : string * Rules.state =
  let name = get_write_name ~state in
  let transition_of_symbol c = Rules.{
      read = c; write = c; action = Right;
      to_state = "get_action_" ^ String.of_char state ^ String.of_char c
    }
  in
  name,
  build_transitions_list transition_of_symbol tape_alphabet

let get_action ~state_symbol:(state, symbol: char * char) : string * Rules.state =
  let name = get_action_name ~state ~symbol in
  let transition_left = 'L', Rules.{
      read = 'L'; write = 'L'; action = Left;
      to_state = go_C_put_name ~state ~symbol ~action:Left
    }
  in
  let transition_right = 'R', Rules.{
      read = 'R'; write = 'R'; action = Right;
      to_state = go_C_put_name ~state ~symbol ~action:Right
    }
  in
  name,
  charhash_of_list [ transition_left; transition_right ]

let go_C_put ~(alphabet: string) ~state_symbol:(state, symbol: char * char) ~(action: Rules.action) : string * Rules.state =
  let name = go_C_put_name ~state ~symbol ~action in
  let destination = Rules.{
      read = 'C'; to_state = put_C_name ~state; write = symbol; action
    }
  in
  name,
  go ~alphabet ~to_state:name ~action:Right ~destination
    (* |RLBEC ^ states (a..z) ^ tape.alphabet *)

let shift_write ~(tape_alphabet: string) ~(blank: char) ~state_symbol:(state, write: char * char) : string * Rules.state =
  let name = shift_write_name ~state ~write in
  let transition_go_F = blank, Rules.{
      read = blank; write; action = Left;
      to_state = go_F_name ~state ~symbol:blank
    }
  in
  let transition_of_symbol c = Rules.{
      read = c; write; action = Right;
      to_state = shift_write_name ~state ~write:c
    }
  in
  name,
  match write with
  | 'E' -> charhash_of_list [ transition_go_F ]
  |  _  -> build_transitions_list transition_of_symbol (tape_alphabet ^ "E")

let generator (rules: Rules.rules) : Rules.rules =
  let assoc : char StringHash.t = Encoder.states_assoc rules in
  let states =
    String.init 
      ( StringHash.to_seq_values assoc |> List.of_seq |> List.sort_uniq compare |> List.length )
      ( fun i -> (int_of_char 'a') + i |> char_of_int )
  in
  (* let states = StringHash.to_seq_values assoc |> List.of_seq in *)
  let name = rules.name ^ "_ception" in
  let alphabet =
      "F|RLBECH"
      ^ states
      ^ rules.alphabet
  in
  let transitions =
    let tape_alphabet = rules.alphabet in
    let blank = rules.blank in
    let go_B =
      let name = "go_B" in
      let to_state = "put_C_" ^ (StringHash.find assoc rules.initial |> String.of_char) in
      let destination = Rules.{
          read = 'B'; to_state; write = 'B'; action = Right
        }
      in
      name,
      go ~alphabet ~to_state:name ~action:Right ~destination
    in
    let get_to_state =
      let name = "get_to_state" in
      let transition_of_symbol c = Rules.{
          read = c; write = c; action = Right;
          to_state = "get_write_" ^ String.of_char c
        }
      in
      name,
      build_transitions_list transition_of_symbol (states ^ "H")
    in
    let fold_state lst (name, rules_state: string * Rules.state) =
      let state = StringHash.find assoc name in
      let init =
        put_C            ~tape_alphabet ~state
        :: put_E                 ~blank ~state
        :: get_write     ~tape_alphabet ~state
        :: lst
      in
      let fold_state_symbol lst (transition: Rules.transition) =
        let state_symbol = (state, transition.read) in
        go_F           ~alphabet             ~state_symbol
        :: go_pipe     ~alphabet             ~state_symbol
        :: get_state                         ~state_symbol
        :: get_read    ~tape_alphabet        ~state_symbol
        :: get_action                        ~state_symbol
        :: go_C_put    ~alphabet             ~state_symbol ~action:Rules.Left
        :: go_C_put    ~alphabet             ~state_symbol ~action:Rules.Right
        :: shift_write ~tape_alphabet ~blank ~state_symbol
        :: lst
      in
      rules_state
      |> CharHash.to_seq_values
      |> Seq.fold_left fold_state_symbol init
    in
    rules.transitions
    |> StringHash.to_seq
    |> Seq.fold_left fold_state [ go_B; get_to_state ]
    |> List.to_seq
    |> StringHash.of_seq
  in
  Rules.{
    name;
    alphabet;
    blank = rules.blank;
    states = "HALT" :: (StringHash.to_seq_keys transitions |> List.of_seq);
    initial = "go_B";
    finals = [ "HALT" ];
    transitions;
  }
