type t = { left: char list; right: char list }

exception Misplaced_cursor of string

let raise_misplaced_cursor () =
  raise @@ Misplaced_cursor "Cursor lost in front of the tape"


let is_begin (tape: t) : bool =
  match tape.left with
  | []       -> raise_misplaced_cursor ()
  | head::[] -> true
  | _        -> false

let is_end (tape: t) : bool =
  match tape.right with
  | [] -> true
  | _  -> false


let read (tape: t) : char =
  match tape.left with
  | []      -> raise_misplaced_cursor ()
  | head::_ -> head

let write (c: char) (tape: t) : t =
  match tape.left with
  | []      -> raise_misplaced_cursor ()
  | _::tail -> { tape with left = c :: tail }

let move (blank: char) (action: Rules.action) (tape: t) : t =
  match action, tape.left, tape.right with
  | Rules.Left,  [], right -> raise_misplaced_cursor ()
  | Rules.Left,  head::[],   right -> { left = blank :: []; right = head :: right }
  | Rules.Left,  head::tail, right -> { left = tail;  right = head :: right }
  | Rules.Right, left, [] -> { tape with left = blank :: left }
  | Rules.Right, left, head::tail -> { left = head :: left; right = tail }


let rec rewind (tape: t) : t =
  match tape.left, tape.right with
  | [], []            -> raise_misplaced_cursor ()
  | head::[], _       -> tape
  | head::tail, right -> rewind { left = tail; right = head :: right }
  | [], head::tail    -> { left = head :: []; right = tail }


let of_string (str: string) : t =
  let f (c: char) (tape: t) : t =
    { left = []; right = c :: tape.right }
  in
  String.fold_right f str { left = []; right = [] }
  |> rewind

let to_string (tape: t) : string =
  (
    tape.left
    |> List.rev
    |> List.to_seq
    |> String.of_seq
  )
  ^
  (
    tape.right
    |> List.to_seq
    |> String.of_seq
  )

let print (tape: t) =
  let rewind = rewind tape in
  let print_char = Printf.printf "%c" in
  match rewind.left with
  | [] -> ()
  | _::_::_ -> failwith "Tape was not rewound"
  | head::[] -> print_char head;
      List.iter print_char rewind.right;
      Printf.printf "\n"
