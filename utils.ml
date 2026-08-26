(* type 'a fmt = (('a, out_channel, unit, unit, unit, unit) format6) *)

let print_err fmt =
	Printf.printf "%!";
	Printf.eprintf ("\027[31m" ^^ fmt ^^ "\027[0m")

let char_to_string c = String.make 1 c
