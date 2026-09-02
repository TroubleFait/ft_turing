module StringHash = Utils.StringHash

val states_assoc   : Rules.rules -> string StringHash.t
val encode_machine : string StringHash.t -> Rules.rules -> string -> string
