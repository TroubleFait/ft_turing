exception Invalid_args_count

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
		|> Rules.parse
		|> Rules.validate
		|> Rules.validate_input input
		|> Rules.is_HALT_reachable
		|> Turing_machine.start_machine input
		|> Tape.print
	end with
	| Invalid_args_count -> print_usage (); exit 1
	| Rules.Invalid_struct -> Rules.print_invalid_struct (); exit 1
  | Sys_error msg
  | Failure msg
  | Lexer.Strings.Malformed msg
  | Lexer.Numbers.Malformed msg
	| Parser.Open_end msg
	| Parser.Tokens.Unexpected msg -> Utils.print_err "%s\n" msg; exit 1
