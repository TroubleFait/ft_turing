(* type 'a fmt = (('a, out_channel, unit, unit, unit, unit) format6) *)

val print_err : ('a, out_channel, unit) format -> 'a

val char_to_string: char -> string

