module CharMap = Map.Make(Char)

type state_kind =
  | Initial
  | Get_to_state
  |  Go_B       of char
  | Put_C       of char
  | Put_E       of char
  |  Go_F       of char * char
  |  Go_pipe    of char * char
  | Get_state   of char * char
  | Get_read    of char * char
  | Shift_write of char * char
  | Get_write   of char
  | Get_action  of char * char
  |  Go_C_put   of char * char * Rules.action

type context = {
  rules:  Rules.rules;
  states: string;
  assoc:  char Utils.StringHash.t
}

let name (state_kind: state_kind) =
  begin
    match state_kind with
    | Initial       -> "initial"
    | Get_to_state  -> "get_to_state"
    |  Go_B       _ -> "go_B_"
    | Put_C       _ -> "put_C_"
    | Put_E       _ -> "put_E_"
    |  Go_F       _ -> "go_F_"
    |  Go_pipe    _ -> "go_|_"
    | Get_state   _ -> "get_state_"
    | Get_read    _ -> "get_read_"
    | Shift_write _ -> "shift_write_"
    | Get_write   _ -> "get_write_"
    | Get_action  _ -> "get_action_"
    |  Go_C_put   _ -> "go_C_put_"
  end
  ^
  begin
    let open String in
    match state_kind with
    | Initial
    | Get_to_state -> empty
    | Go_B  (state)
    | Put_C (state)
    | Put_E (state) -> of_char state
    | Go_F        (state, symbol)
    | Go_pipe     (state, symbol)
    | Get_state   (state, symbol)
    | Get_read    (state, symbol)
    | Shift_write (state, symbol) -> of_char state ^ of_char symbol
    | Get_write  (to_state)                -> of_char to_state
    | Get_action (to_state, write)         -> of_char to_state ^ of_char write
    | Go_C_put   (to_state, write, action) -> of_char to_state ^ of_char write ^ of_char (Encoder.char_of_action action)
  end

let raise_invalid_action symbol =
  failwith @@ "'" ^ String.of_char symbol ^ "': invalid symbol for action (expected 'L' or 'R')"

let make_transition ~(blank:char) (read, state_kind: char * state_kind) =
  let write symbol action to_state =
    Rules.transition_of (read, name to_state, symbol, action )
  in
  let next = write read Right in
  let go action = write read action state_kind in
  match read, state_kind  with
  | 'H', Initial                        -> Rules.transition_of ('H', "HALT", 'H', Right)
  |  s , Initial                        -> next            @@ Go_B s
  | 'B', Go_B (s)                       -> next            @@ Put_C s
  | _,   Go_B _                         -> go Right
  | 'B', Put_C (s)                      -> next            @@ Shift_write (s, 'C')
  | 'E', Put_C (s)                      -> write 'C' Right @@ Put_E s
  |  r , Put_C (s)                      -> write 'C' Left  @@ Go_F (s, r)
  | 'E', Shift_write (s, w)             -> write  w  Right @@ Put_E s
  |  r , Shift_write (s, w)             -> write  w  Right @@ Shift_write (s, r)
  | _,   Put_E (s)                      -> write 'E' Left  @@ Go_F (s, blank)
  | 'F', Go_F  (s, r)                   -> next            @@ Go_pipe (s, r)
  | _,   Go_F _                         -> go Left
  | '|', Go_pipe (s, r)                 -> next            @@ Get_state (s, r)
  | _,   Go_pipe _                      -> go Right
  | _,   Get_state (s, r) when read = s -> next            @@ Get_read (s, r)
  | _,   Get_state (s, r)               -> next            @@ Go_pipe (s, r)
  | _,   Get_read  (s, r) when read = r -> next            @@ Get_to_state
  | _,   Get_read  (s, r)               -> next            @@ Go_pipe (s, r)
  |  ts, Get_to_state                   -> next            @@ Get_write ts
  |  w , Get_write  (ts)                -> next            @@ Get_action (ts, w)
  | 'L', Get_action (ts,  w)            -> next            @@ Go_C_put (ts, w, Left)
  | 'R', Get_action (ts,  w)            -> next            @@ Go_C_put (ts, w, Right)
  |  a , Get_action (ts,  w)            -> raise_invalid_action a
  | 'C', Go_C_put   ('H', w, a)         -> Rules.transition_of ( 'C', "HALT", w, a )
  | 'C', Go_C_put   (ts,  w, a)         -> write w a       @@ Put_C ts
  | _,   Go_C_put _                     -> go Right

let state_of ~(blank:char) transition alphabet : string * Rules.state =
  name transition,
  alphabet
  |> String.fold_left (fun acc read ->
    (read, make_transition ~blank (read, transition)) :: acc ) []
  |> List.to_seq
  |> Utils.CharHash.of_seq

let to_state_map assoc transitions : Rules.action list CharMap.t CharMap.t =
  let char_of_state = Utils.StringHash.find assoc in
  Utils.StringHash.fold (fun s state acc ->
    Utils.CharHash.fold (fun _ (state_kind: Rules.transition) acc ->
      (char_of_state state_kind.to_state, (state_kind.write, state_kind.action)) :: acc ) state acc
    ) transitions []
  |> List.fold_left (fun map (to_state, (w, a)) ->
    CharMap.add_to_list to_state (w, a) map) CharMap.empty
  |> CharMap.map (fun combinations ->
    List.fold_left (fun write_map (w, a) ->
      CharMap.add_to_list w a write_map
    ) CharMap.empty combinations)

let string_of_list lst = lst |> List.to_seq |> String.of_seq

let to_states to_state_map =
  CharMap.fold (fun ts _ l -> ts :: l) to_state_map [] |> string_of_list

let writes write_map =
  CharMap.fold (fun w _ l -> w :: l) write_map [] |> string_of_list

let actions action_list =
  String.init (List.length action_list) (fun i -> List.nth action_list i |> Encoder.char_of_action)

let list_states (context: context) =
  let states, tape = context.states, context.rules.alphabet in
  let state_of = state_of ~blank:context.rules.blank in
  let by_states make_state lst =
    String.fold_left (fun acc s -> make_state s :: acc) lst states
  in
  let by_tape make_state lst =
    String.fold_left (fun acc tp ->
      by_states (make_state tp) acc) lst tape
  in
  let to_state_map = to_state_map context.assoc context.rules.transitions in
  let by_to_state make_state =
    CharMap.fold (fun ts write_map -> make_state ts write_map) to_state_map
  in
  let by_write make_state =
    by_to_state (fun ts write_map ->
      CharMap.fold (make_state ts) write_map)
  in
  let by_action make_state =
    by_write (fun ts w action_list lst ->
      List.fold_left (fun lst a -> make_state ts w a lst) lst action_list)
  in
  [                       state_of Initial                (states ^ "H") ]
  |> by_states  (fun s -> state_of (Go_B  s)              (states ^ tape ^ "F|HLRB"))
  |> by_states  (fun s -> state_of (Put_C s)              (tape ^ "BE"))
  |> by_states  (fun s -> state_of (Put_E s)              "_")
  |> by_states  (fun s -> state_of (Shift_write (s, 'C')) (tape ^ "E"))
  |> by_tape (fun tp s -> state_of (Shift_write (s, tp))  (tape ^ "E"))
  |> by_tape (fun tp s -> state_of (Go_F (s, tp))         (states ^ tape ^ "F|HLRBC"))
  |> by_tape (fun tp s -> state_of (Go_pipe (s, tp))      (states ^ tape ^ "|HLR"))
  |> by_tape (fun tp s -> state_of (Get_state (s, tp))     states)
  |> by_tape (fun tp s -> state_of (Get_read (s, tp))      tape)
  |> List.cons                                      @@ state_of  Get_to_state         (to_states to_state_map)
  |> by_to_state (fun ts write_map     -> List.cons @@ state_of (Get_write ts)        (writes write_map))
  |> by_write    (fun ts w action_list -> List.cons @@ state_of (Get_action (ts, w))  (actions action_list))
  |> by_action   (fun ts w a           -> List.cons @@ state_of (Go_C_put (ts, w, a)) (states ^ tape ^ "|HLRBC"))
