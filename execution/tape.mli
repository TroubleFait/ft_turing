type t = { left: char list; right: char list}

exception Misplaced_cursor of string

val raise_misplaced_cursor : unit -> unit

val is_begin   : t -> bool
val is_end     : t -> bool

val read       : t -> char
val write      : char -> t -> t
val move       : char -> Rules.action -> t -> t

val rewind     : t -> t

val of_string  : string -> t
val to_string  : t -> string
val print      : t -> unit
