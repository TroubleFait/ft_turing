exception Not_enough_args
exception Too_many_args

let print_usage () =
	Printf.printf
"usage: ft_turing [-h] jsonfile input

positional arguments:
  jsonfile            json description of the machine

  input               input of the machine

optional arguments:
  -h, --help          show this help message and exit\n"

let fetch_argv () : (bool * string * string) =
	let len = Array.length Sys.argv in
	if len < 2 then raise Not_enough_args;
	let has_help = Sys.argv.(1) = "-h" || Sys.argv.(1) = "--help" in
	if len < (if has_help then 4 else 3) then raise Not_enough_args;
	if len > (if has_help then 4 else 3) then raise Too_many_args;
	let json_file = Sys.argv.(if has_help then 2 else 1) in
	let input     = Sys.argv.(if has_help then 3 else 2) in
	(has_help, json_file, input)

let () =
	try begin
		let help, json_file, input = fetch_argv () in
		if help then print_usage ();
	  (* let machine_spec = Parser.parse json_file in *)
		(* Parse the JSON file and input *)
	end with
	| Not_enough_args
	| Too_many_args -> print_usage (); exit 1
