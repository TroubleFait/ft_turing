module StringHash = Utils.StringHash

val char_of_action : Rules.action -> char

val states_assoc   : Rules.rules -> char StringHash.t
val encode_machine : Rules.rules -> string -> string
