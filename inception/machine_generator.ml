module CharHash   = Utils.CharHash
module StringHash = Utils.StringHash

type action = Rules.action

module CharMap   = Map.Make(Char)
module StringMap = Map.Make(String)

let generator (rules: Rules.rules) : Rules.rules =
  let assoc : char StringHash.t = Encoder.states_assoc rules in
  let states =
    assoc |> Utils.StringHash.to_seq_values |> List.of_seq
    |> List.sort_uniq compare 
    |> List.filter (fun c -> c <> 'H') |> List.to_seq |> String.of_seq
  in
  let name = rules.name ^ "_ception" in
  let alphabet =
      "F|RLBECH"
      ^ states
      ^ rules.alphabet
      ^ "_"
  in
  let transitions =
    States.list_states States.{ rules; states; assoc }
    |> List.to_seq |> StringHash.of_seq
  in
  Rules.{
    name;
    alphabet;
    blank = '_';
    states = "HALT" :: (StringHash.to_seq_keys transitions |> List.of_seq);
    initial = States.name States.Initial;
    finals = [ "HALT" ];
    transitions;
  }
