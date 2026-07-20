module PrintJSON = struct
  type indent_t = int

  let indent_to_string indent = String.make indent '\t'

  let line_of_string indent str = indent_to_string indent ^ str ^ "\n"

  (* let object_to_string indent obj =
     *)

  let rec array_to_string indent arr =
    let len = Array.length arr in
    let rec loop i =
      let str = to_string (indent + 1) arr.(i) in
      if i + 1 >= len then
        str
        ^ (line_of_string indent "]")
      else
        (String.sub str 0 @@ String.length str - 1) ^ ",\n"
        ^ loop (i + 1)
    in
    (line_of_string indent "[") ^ loop 0

  and to_string indent (data : Parser.json) =
    match data with
    | [] -> ""
    | head :: tail -> begin
      match data with
      (* | Object o -> object_to_string indent o *)
      | Array a  -> array_to_string  indent a
      | Number f -> line_of_string   indent @@ string_of_float f
      | String s -> line_of_string   indent s
      | Bool b   -> line_of_string   indent @@ string_of_bool b
      | Null     -> line_of_string   indent "null"
    end
    ^ to_string indent tail

    let print (data : Parser.json) =
      print_string @@ to_string 0 data
end

let () =
  if Array.length Sys.argv <> 2 then
    print_endline "Usage:";
    print_endline (Sys.argv.(0) ^ " <file.json>");
	try begin
    let json_str = Read_file.string_of_file Sys.argv.(1) in
    let data = Parser.parse @@ Lexer.lex json_str in
    print data
  end with
  | Sys_error message
  | Failure message -> prerr_endline message
