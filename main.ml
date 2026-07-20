module PrintJSON = struct
  type indent_t = int

  let indent_to_string indent = String.make indent '\t'

  let array_to_string arr indent =
    let len = Array.length arr in
    let rec loop i =
      to_string arr.(i) (indent + 1) :: if 
    in
    ((indent_to_string indent) ^ "[") ::

  let to_string (data : json) ?(indent = 0) = function
  | Array arr -> 
end

(* 
module Values = struct
  type t =
  | Object of t StringMap.t
  | Array of t list (* or array? *)
  | Number of float
  | String of string
  | Bool of bool
  | Null
*)

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
