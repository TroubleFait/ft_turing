let printer (rules: Rules.rules) : string =
  let name = rules.name in
  let print_string str = "\"" ^ str ^ "\"" in
  let print_char c = print_string @@ String.of_char c in
  let field indent name content =
    indent ^ (print_string name) ^ ": " ^ content
  in
  let print_transition (transition: Rules.transition) =
    "      { "
    ^ field "" "read" (print_char transition.read)
    ^ field ", " "to_state" (print_string transition.to_state)
    ^ field ", " "write" (print_char transition.write)
    ^ field ", " "action" (Rules.action_to_str transition.action |> print_string)
    ^ " }"
  in
  let print_state (state: Rules.state) =
    "[\n"
    ^ (
    state
    |> Utils.CharHash.to_seq_values
    |> List.of_seq
    |> List.map print_transition
    |> String.concat ",\n"
    )
    ^ "\n    ]"
  in
  let print_list indent to_string lst =
    if String.is_empty indent then
      "[ "
      ^ ( List.map to_string lst |> String.concat ", " )
      ^ " ]"
    else
      "[\n  " ^ indent
      ^ ( List.map to_string lst |> String.concat (",\n  " ^ indent) )
      ^ "\n" ^ indent ^ "]"
  in
  let print_object indent to_string lst =
    "{\n"
    ^ ( List.map ( fun (name, content) ->
        indent ^ field "  " name ( to_string content )
    ) lst |> String.concat ",\n" )
    ^ "\n"
    ^ indent ^ "}"
  in
  let string_list =
       field "  " "name" ( print_string name )
    :: field "  " "alphabet" ( print_list "" print_string
          ( String.split_all ~sep:"" ~drop:(String.is_empty) rules.alphabet ) )
    :: field "  " "blank" ( print_char rules.blank )
    :: field "  " "states" ( print_list "  " print_string rules.states )
    :: field "  " "initial" ( print_string rules.initial )
    :: field "  " "finals" ( print_list "" print_string rules.finals )
    :: field "  " "transitions" ( print_object "  " print_state
          ( rules.transitions |> Utils.StringHash.to_seq |> List.of_seq ) )
    :: []
  in
  "{\n" ^ (String.concat ",\n" string_list) ^ "\n}\n"
  
let file_of_string ~(name: string) (content: string) : unit =
  let oc = open_out name in
  output_string oc content;
  Printf.printf "%s created.\n" name
