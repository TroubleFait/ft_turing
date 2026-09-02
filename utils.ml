(* type 'a fmt = (('a, out_channel, unit, unit, unit, unit) format6) *)

module StringHash = Hashtbl.Make(String)
module CharHash   = Hashtbl.Make(Char)

let print_err fmt =
	Printf.printf "%!";
	Printf.eprintf ("\027[31m" ^^ fmt ^^ "\027[0m")

let rec find_aux_not_of (len: int) (chars: string) (str: string) (comp) (op) = function
	| i when comp i len -> None
	| i ->
	match String.contains chars str.[i] with
	| true -> find_aux_not_of len chars str (comp) (op) (op i 1)
	| false -> Some i

let find_first_not_of (chars: string) (str: string): int option =
	find_aux_not_of (String.length str) chars str (>=) (+) 0

let find_last_not_of (chars: string) (str: string): int option =
	let len = String.length str in
	find_aux_not_of 0 chars str (<) (-) (len - 1)

let trim (chars: string) (str: string): string =
	match find_first_not_of chars str with
	| None -> ""
	| Some first ->
	match find_last_not_of chars str with
	| None -> ""
	| Some last ->
	String.sub str first (last - first + 1)

