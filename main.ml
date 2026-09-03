exception Invalid_args_count

let print_usage () =
  Printf.printf
"usage: ft_turing [-h] jsonfile input

positional arguments:
  jsonfile            json description of the machine

  input               input of the machine

optional arguments:
  -h, --help          show this help message and exit

  -i, --inception     generates a turing machine that can run the inputed machine,
                      encodes the inputed machine with its tape,
                      and runs it
\n"

type flag = None | Help | Inception

let fetch_argv () : (flag * string * string) =
  match Sys.argv with (* (flag, json_file, input) *)
  | [| _; "-h"; json_file; input|] | [| _; "--help";      json_file; input|] -> (Help,      json_file, input)
  | [| _; "-i"; json_file; input|] | [| _; "--inception"; json_file; input|] -> (Inception, json_file, input)
  | [| _; json_file; input |]                                                -> (None,      json_file, input)
  | _ -> raise Invalid_args_count

let () =
  try begin
    let flag, json_file, input = fetch_argv () in
    let rules =
      json_file
      |> Read_file.string_of_file
      |> Lexer.lex
      |> Parser.parse 
      |> Rules.parse
      |> Rules.validate
      |> Rules.validate_input input
      |> Rules.is_HALT_reachable
    in
    let launch rules input =
      Turing_machine.start_machine input rules
      |> Tape.print
    in
    match flag with
    | Help -> print_usage (); launch rules input
    | None -> launch rules input
    | Inception ->
      let rules_ception = Machine_generator.generator rules in
      let encoded       = Encoder.encode_machine rules input in
      Write_file.printer rules_ception
      |> Write_file.file_of_string ~name:(rules_ception.name ^ ".json");
      Printf.printf "%s.json created.\n" rules_ception.name;
      Printf.printf "%s encoded as a tape:\n" rules.name;
      Printf.printf "%s\n\n" encoded;
      launch rules_ception encoded
  end with
    | Invalid_args_count -> print_usage (); exit 1
    | Rules.Invalid_struct -> Rules.print_invalid_struct (); exit 1
    | Sys_error msg
    | Failure msg
    | Lexer.Strings.Malformed msg
    | Lexer.Numbers.Malformed msg
    | Parser.Open_end msg
    | Parser.Tokens.Unexpected msg -> Utils.print_err "%s\n" msg; exit 1
