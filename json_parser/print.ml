type indent_t = int

let indent_to_string indent = String.make indent '\t'

let line_of_string indent str = indent_to_string indent ^ str ^ "\n"

let rec object_to_string indent obj =
  let len = Utils.StringHash.length obj in
  let arr = Utils.StringHash.to_seq obj |> Array.of_seq in
  let rec loop i =
    if i >= len then (line_of_string indent "}") else
    let value_str = to_string (indent + 2) @@ snd arr.(i) in
    let str = line_of_string (indent + 1) @@ fst arr.(i) ^ ":\n" ^ value_str in
    (String.sub str 0 @@ String.length str - 1) ^ ",\n"
      ^ loop (i + 1)
  in
  (line_of_string indent "{") ^ loop 0

and array_to_string indent arr =
  let len = Array.length arr in
  let rec loop i =
    if i >= len then (line_of_string indent "]") else
    let str = to_string (indent + 1) arr.(i) in
    (String.sub str 0 @@ String.length str - 1) ^ ",\n"
      ^ loop (i + 1)
  in
  (line_of_string indent "[") ^ loop 0

and to_string indent (data : Parser.json) =
  match data with
  | Empty -> ""
  | Object o -> object_to_string indent o
  | Array a  -> array_to_string  indent a
  | Number f -> line_of_string   indent @@ string_of_float f
  | String s -> line_of_string   indent s
  | Bool b   -> line_of_string   indent @@ string_of_bool b
  | Null     -> line_of_string   indent "null"

let print (data : Parser.json) =
  print_string @@ to_string 0 data