let print data =
  print_endline "print data here"

let () =
  if Array.length Sys.argv <> 2 then
    print_endline "Usage:";
    print_endline (Sys.argv.(0) ^ " <file.json>")
	try begin
    let json_str = Read_file.string_of_file Sys.argv.(1) in
    let data = Parser.parse @@ Lexer.lex json_str in
    print data
  end with
  | Sys_error message
  | Failure message -> prerr_endline message
