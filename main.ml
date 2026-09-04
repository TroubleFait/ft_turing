exception Invalid_args_count

let print_usage () =
  Printf.printf
"usage: ft_turing [-h] jsonfile input

positional arguments:
  jsonfile            json description of the machine

  input               input of the machine

optional arguments:
  -h, --help          show this help message and exit

  -w, --window=NUM    size of the window on which to print the tape
                      as it is traversed (default: 60)

  -s, --skip          do not show operations where the machine didn't write anything
                      and th state didn't change

  -i, --inception     generates a turing machine that can run the inputed machine,
                      encodes the inputed machine with its tape,
                      and runs it
\n"

type flags = {
  help:      bool;
  skip:      bool;
  inception: bool;
  window:    int;
}

let fetch_argv () : (flags * string * string) =
  let argc = Array.length Sys.argv in
  let fetch_window str =
    let failmsg = "\"" ^ str ^ "\": not a valid window size" in
    let window =
      try int_of_string str with Failure msg -> failwith failmsg
    in
    if window < 0 then failwith failmsg;
    window
  in
  let rec fetch_flags (flags: flags) i =
    if i >= argc then
      flags, i
    else match Sys.argv.(i) with
      | "-h" | "--help"      -> fetch_flags { flags with help      = true } (i + 1)
      | "-s" | "--skip"      -> fetch_flags { flags with skip      = true } (i + 1)
      | "-i" | "--inception" -> fetch_flags { flags with inception = true } (i + 1)
      | "-w" ->
        fetch_flags { flags with window =
          try fetch_window Sys.argv.(i + 1) with
          | Invalid_argument msg -> raise Invalid_args_count } (i + 2)
      | s when String.starts_with ~prefix:"--window=" s ->
        fetch_flags { flags with window = fetch_window @@ String.sub Sys.argv.(i) 9 (String.length Sys.argv.(i) - 9) } (i + 1)
      | _ -> flags, i
  in
  let flags, i = fetch_flags {
      help = false; skip = false; inception = false;
      window = 60
    } 1
  in
  if argc <> i + 2 then
    raise Invalid_args_count
  else
    flags, Sys.argv.(i), Sys.argv.(i + 1)

let () =
  try begin
    let flags, json_file, input = fetch_argv () in
    if flags.help then print_usage ();
    let rules_from_file =
      json_file
      |> Read_file.string_of_file
      |> Lexer.lex
      |> Parser.parse 
      |> Rules.parse
      |> Rules.validate
      |> Rules.validate_input input
      |> Rules.is_HALT_reachable
    in
    let rules, tape =
      if flags.inception then
        let rules_ception = Machine_generator.generator rules_from_file in
        Write_file.printer rules_ception
        |> Write_file.file_of_string ~name:("machines/" ^ rules_ception.name ^ ".json");
        rules_ception,
        Encoder.encode_machine rules_from_file input
      else
        rules_from_file, input
    in
    let final_tape =
      Turing_machine.start_machine
        ~flags:{ skip = flags.skip; window = flags.window }
        rules
        tape
    in
    Printf.printf "\n";
    Tape.print final_tape
  end with
    | Invalid_args_count -> print_usage (); exit 1
    | Rules.Invalid_struct -> Rules.print_invalid_struct (); exit 1
    | Sys_error msg
    | Failure msg
    | Lexer.Strings.Malformed msg
    | Lexer.Numbers.Malformed msg
    | Parser.Open_end msg
    | Parser.Tokens.Unexpected msg -> Utils.print_err "%s\n" msg; exit 1
