exception Invalid_args_count

module PrintJSON = struct
  type indent_t = int

  let indent_to_string indent = String.make indent '\t'

  let line_of_string indent str = indent_to_string indent ^ str ^ "\n"

  let rec object_to_string indent obj =
    let len = Parser.StringMap.cardinal obj in
    let arr = Parser.StringMap.to_seq obj |> Array.of_seq in
    let rec loop i =
      if i >= len then (line_of_string indent "}") else
      let value_str = to_string (indent + 2) @@ snd arr.(i) in
      let str = line_of_string (indent + 1) @@ fst arr.(i) ^ ":\n" ^ value_str in
      (String.sub str 0 @@ String.length str - 1) ^ ",\n"
        ^ loop (i + 1)
    in
    (line_of_string indent "{") ^ loop 0

  and array_to_string indent arr =
    let len = Array.length arr in
    let rec loop i =
      if i >= len then (line_of_string indent "]") else
      let str = to_string (indent + 1) arr.(i) in
      (String.sub str 0 @@ String.length str - 1) ^ ",\n"
        ^ loop (i + 1)
    in
    (line_of_string indent "[") ^ loop 0

  and to_string indent (data : Parser.json) =
    match data with
    | Empty -> ""
    | Object o -> object_to_string indent o
    | Array a  -> array_to_string  indent a
    | Number f -> line_of_string   indent @@ string_of_float f
    | String s -> line_of_string   indent s
    | Bool b   -> line_of_string   indent @@ string_of_bool b
    | Null     -> line_of_string   indent "null"

  let print (data : Parser.json) =
    print_string @@ to_string 0 data
end

let print_usage () =
	Printf.printf
"usage: ft_turing [-h] jsonfile input

positional arguments:
  jsonfile            json description of the machine

  input               input of the machine

optional arguments:
  -h, --help          show this help message and exit\n"

let fetch_argv () : (bool * string * string) = match Sys.argv with (* (has_help, json_file, input) *)
	| [| _; "-h"; json_file; input|] | [| _; "--help"; json_file; input|] -> (true, json_file, input)
	| [| _; json_file; input |] -> (false, json_file, input)
	| _ -> raise Invalid_args_count

let () =
	try begin
		let help, json_file, input = fetch_argv () in
		if help then print_usage ();
    let json_str = Read_file.string_of_file json_file in
    Parser.parse @@ Lexer.lex json_str
		|> Rules_parser.parse_rules |> Rules_parser.validate_rules |> Rules_parser.validate_input input
		|> Turing_machine.start_machine input |> Printf.printf "%s\n"
	end with
  | Sys_error message
  | Failure message -> prerr_endline message; exit 1
	| Invalid_args_count -> print_usage (); exit 1
	| Rules_parser.Invalid_struct -> Rules_parser.print_invalid_struct (); exit 1
	| Parser.Open_end msg -> Utils.print_err "%s\n" msg; exit 1
	| Parser.Tokens.Unexpected msg -> Utils.print_err "Unexpected Token: %s\n" msg; exit 1
