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
		Printf.printf "%s\n" (Turing_machine.start_machine "");
		(* let machine_spec = Parser.parse json_file in *)
		(* Parse the JSON file and input *)
	end with
	| Invalid_args_count -> print_usage (); exit 1
