module StringHash = Utils.StringHash

val states_assoc   : Rules.rules -> char StringHash.t
val encode_machine : Rules.rules -> string -> string
