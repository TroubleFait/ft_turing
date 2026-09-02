module CharHash   = Utils.CharHash
module StringHash = Utils.StringHash

type action = Rules.action

let go (alphabet: string) (state, std_action: string * action) (read, to_state, write, action: char * string * char * action) : Rules.state =
  String.fold_left ( fun acc c -> ( c,
      if c = read then
        (* { read = read; to_state = to_state; write = write; action = action } *)
        { read; to_state; write; action }
      else
        { read = c; to_state = state; write = c; action = std_action }
    ) :: acc
  ) acc alphabet
  |> List.to_seq
  |> CharHash.of_seq

let get (alphabet: string) (to_state: string) (target: (char * string) option) =
  let fold = match target with
  | Some r, s -> ( fun acc c -> ( c, {
        to_state = if c = r then s else to_state;
        read = c; write = c; action = Right;
      } ) :: acc )
  | None -> ( fun acc c -> ( c, {
        read = c; to_state = to_state; write = c; action = Right
      } ) :: acc )
  in
  String.fold_left fold acc alphabet
  |> List.to_seq
  |> CharHash.of_seq

let put_C (states_alphabet: string) (state: char) =
  ( 'B', {
    read = 'B'; write = 'B'; action = Right;
    to_state = "shift_write_C_" ^ String.of_char state
  } )
  :: String.fold_left ( fun acc c -> ( c, {
        read = c; write = 'C'; action = Left;
        to_state = "go_F_get_" ^ String.of_char state ^ String.of_char c
      } ) :: acc
  ) acc states_alphabet
  |> List.to_seq
  |> CharHash.of_seq

let go_F (alphabet: string) (state, symbol: char * char) =
  let str_front = "go_F_get_" ^ String.of_char state ^ String.of_char symbol in
  let str_pipe  = "go_|_get_" ^ String.of_char state ^ String.of_char symbol in
  go alphabet (str_front, Left) ('F', str_pipe, 'F', Right)

let go_pipe (alphabet: string) (state, symbol: char * char) =
  let str_pipe      = "go_|_get_"  ^ String.of_char state ^ String.of_char symbol in
  let str_get_state = "get_state_" ^ String.of_char state ^ String.of_char symbol in
  (* no 'F' in alphabet *)
  go alphabet (str_pipe, Right) ('|', str_get_state, '|', Right)

let get_state (state, symbol: char * char) =
  let alphabet = String.init (int_of_char state - int_of_char 'a' + 1)
    (fun i -> i + int_of_char 'a' |> char_of_int)
  in
  let to_state = "get_read_" ^ String.of_char state ^ String.of_char symbol in
  (* get alphabet None to_state *)
  get alphabet       (* Maybe? *)
    Some (state, to_state)
    ( "go_|_get_"  ^ String.of_char state ^ String.of_char symbol )

let get_read (tape_alphabet: string) (state, symbol: char * char) =
  get tape_alphabet
    Some (symbol, "get_to_state")
    ( "go_|_get_" ^ String.of_char state ^ String.of_char symbol )

let get_write (tape_alphabet: string) (state: char) =
  String.fold_left ( fun acc c -> ( c, {
        read = c; write = c; action = Right;
        to_state = "get_action_" ^ String.of_char state ^ String.of_char c
      } ) :: acc
  ) acc tape_alphabet
  |> List.to_seq
  |> CharHash.of_seq

let get_action (state, symbol: char * char) =
  ('R', { read = 'R'; write = 'R'; action = Right;
    "go_C_put_" ^ String.of_char state ^ String.of_char symbol ^ "R"
  })
  :: ('L', { read = 'L'; write = 'L'; action = Left;
    "go_C_put_" ^ String.of_char state ^ String.of_char symbol ^ "L"
  })
  |> List.to_seq
  |> CharHash.of_seq

let go_C_put (alphabet: string) (state, symbol, action: char * char * action) =
  let str_go_C_put =
    "go_C_put_" ^ String.of_char state ^ String.of_char symbol ^ action_to_str
  in
  go alphabet (str_go_C_put, Right) ('C', "put_C_" ^ String.of_char state, symbol, action)
    (* |RLBEC ^ states (a..z) ^ tape.alphabet *)

let shift_write (tape_alphabet: string) (blank: char) (write: char) (state: char) =
  let str_mem = "_" ^ String.of_char state in
  ('B', { read = 'B'; write = write; action = Left; to_state = "go_F_get" ^ str_mem ^ "_" ^ String.of_char blank })
  :: String.fold_left ( fun acc c -> ( c, {
        read = c; write = write; action = Right;
        (* read = c; write; action = Right; *)
        to_state = "shift_write_" ^ String.of_char c ^ str_mem
      } ) :: acc
  ) acc tape_alphabet
  |> List.to_seq
  |> CharHash.of_seq

let generator (rules: Rules.rules) : Rules.rules * string =
  let assoc = Encoder.states_assoc rules in
  let assoc_sates =
    String.init 
      ( StringHash.to_seq_values assoc |> List.of_seq |> List.sort_uniq compare |> List.length )
      ( fun i -> (int_of_char 'a') + i |> char_of_int )
  in
  let alphabet =
      "F|RLBECH"
      ^ assoc_sates
      ^ rules.alphabet
  in
  let transitions =
    let go_B =
      let str_put_C = ("put_C_" ^ StringHash.find assoc rules.initial) Right in
      go alphabet ("go_B", Right) ('B', str_put_C, 'B', Right)
    in
    let get_to_state =
      String.fold_left ( fun acc c ->
        ( c, {
            read = c; write = c; action = Right;
            to_state = "get_write_" ^ String.of_char c
          } ) :: acc
      ) acc (assoc_sates ^ "H")
      |> List.to_seq
      |> CharHash.of_seq
    in
    let 
  in
  {
    name = rules.name ^ "_ception";
    alphabet = alphabet;
    blank = rules.blank;
    states = "HALT" :: StringHash.to_seq_keys transitions |> List.of_seq;
    initial = "go_B";
    finals = [ "HALT" ];
    transitions = transitions;
  }
