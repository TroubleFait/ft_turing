type transition =
  | Initial
  | Get_to_state
  |  Go_B       of char
  | Put_C       of char
  | Put_E       of char
  |  Go_F       of char * char
  |  Go_pipe    of char * char
  | Get_state   of char * char
  | Get_read    of char * char
  | Shift_write of char * char
  | Get_write   of char
  | Get_action  of char * char
  |  Go_C_put   of char * char * Rules.action

type context = {
  rules:  Rules.rules;
  states: string;
  assoc:  char Utils.StringHash.t
}

val name            : transition -> string
val make_transition : char * transition -> Rules.transition

val list_states     : context -> (string * Rules.state) list
