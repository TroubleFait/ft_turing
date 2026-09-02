(* type 'a fmt = (('a, out_channel, unit, unit, unit, unit) format6) *)

module StringHash = Hashtbl.Make(String)
module CharHash   = Hashtbl.Make(Char)

val print_err : ('a, out_channel, unit) format -> 'a

val trim: string -> string -> string

val find_first_not_of: string -> string -> int option
val find_last_not_of: string -> string -> int option