type nat = Zero | Succ of nat

type t = Left of int | Right of int
let double_right = function
	| Left i -> i
	| Right i -> 2*i

(* type 'a node = {value:'a; next:my_list } and mylist = Nil | Node of 'a node *)
(* type 'a my_list_parameterized = Nil | CoP of 'a * 'a my_list_parameterized *)
type typeTest = A of int | B of string | C of bool
type my_list = Nil | Co of typeTest * my_list

let () =
	let test_lst = Co (A 5, Co(B "a", Co(C false, Nil))) in
(* 	let lsst:node = {value: 5; next: Nil} in *)
(* 	let test_lst_parameterized = CoP (5, CoP(6, Nil)) in *)

	let clamp1 v mi ma = max mi (min v ma) in
	let clamp2 v mi ma = v |> min ma |> max mi in
	let ( |- ) x y = max x y in
	let ( -| ) x y = min x y in
	let ( |> ) x f = f x in

(*
List.hd (List.tl lst)
List.(hd (tl lst))
lst |> List.tl |> List.hd
List.(lst |> tl |> hd)
List.nth lst 1
(lst |> List.nth) 1
*)

let rec lte lst =
	if lst |> List.length <= 1 then
		true
	else if lst |> List.length = 2 then
		lst |> List.hd <= ((lst |> List.nth) 1)
	else if lst |> List.hd > (lst |> List.tl |> List.hd) then
		false
	else lte (lst |> List.tl)
in

let rec lte2 = function
	| [] | _::[] -> true
	| h1::h2::[] -> h1 <= h2
	| h1::h2::_ when h1 > h2 -> false
	| _::t -> lte t
in

let rec print_lst_fst_to_last lst =
	if lst |> List.length >= 2 then begin
		Printf.printf "%d, " (lst |> List.hd);
		print_lst_fst_to_last (lst |> List.tl)
	end
	else if lst |> List.length = 1 then
		Printf.printf "%d\n" (lst |> List.hd)
	else
		Printf.printf "Empty\n"
in

let rec print_lst_fst_to_last2 = function
	| [] -> Printf.printf "Empty\n"
	| [h] -> Printf.printf "%d\n" h
	| h::t -> begin Printf.printf "%d, " h; print_lst_fst_to_last2 t end
in

let print_lst_last_to_fst lst =
	let rec go lst i =
		if i > 0 then begin
			Printf.printf "%d, " (List.nth lst i);
			go lst (i - 1)
		end
		else
			Printf.printf "%d\n" (List.nth lst i)
	in
	if lst |> List.length <= 0 then
		Printf.printf "Empty\n"
	else
		go lst List.(length lst - 1)
in

let print_lst_last_to_fst2 lst =
	match lst with
	| [] -> Printf.printf "Empty\n"
	| _::_ -> begin
		let rec go lst i =
			match i with
				| 0 -> Printf.printf "%d\n" (List.nth lst i)
				| _ -> Printf.printf "%d, " (List.nth lst i); go lst (i - 1)
		in
		go lst List.(length lst - 1)
	end
in


let sum lst =
	let rec go lst acc =
		match lst with
		| [] -> acc
		| h::t -> go t (acc + h)
	in
	go lst 0
in

let rec append_list lst1 lst2 =
	match lst1 with
	| [] -> lst2
	| h::t -> h::(append_list t lst2)
in

let append_list2 lst1 lst2 =
	let rec go lst1 lst2 = function
		| 0 -> lst2
		| i -> go lst1 ((List.nth lst1 (i - 1))::lst2) (i - 1)
	in
	go lst1 lst2 (List.length lst1)
in

(* let rec print_my_list_parameterized = function *)
(* 		| Nil -> Printf.printf "Nil\n" *)
(* 		| CoP (h, t) -> begin Printf.printf "v->"; print_my_list_parameterized t end *)
(* in *)

let rec print_my_list = function
		| Nil -> Printf.printf "Nil\n"
		| Co (A h, t) -> begin Printf.printf "%d->" h; print_my_list t end
		| Co (B h, t) -> begin Printf.printf "%s->" h; print_my_list t end
		| Co (C h, t) -> begin Printf.printf "%b->" h; print_my_list t end
in

let map f lst =
	let rec go acc f = function
		| Nil -> acc
		| Co (h, t) -> go (Co(f h, acc)) f t
	in
	go Nil f lst
in

let rec and_rec = function
	| [] -> true
	| h::_ when not h -> false
	| h::t -> and_rec t
in

	Printf.printf "comparison test: %b, lte2: %b\n" (lte [0;5;10;15;15]) (lte2 [0;5;10;15;15]);
	Printf.printf "clamp1: %d, clamp2: %d, clamp op: %d\n" (clamp1 10 1 9) (clamp2 0 1 9) (1 |- 11 -| 9);
	print_lst_fst_to_last [0;1;2;3;4;5];
	print_lst_fst_to_last2 [0;1;2;3;4;5];
	print_lst_last_to_fst [0;1;2;3;4;5];
	print_lst_last_to_fst2 [0;1;2;3;4;5];
	print_lst_fst_to_last (2::[3;4]);
	Printf.printf "sum: %d\n" (sum [0;1;2;3;4;5]);
	print_lst_fst_to_last (append_list [1;2] [3;4]);
	print_lst_fst_to_last (append_list2 [1;2] [3;4]);

	Printf.printf "test pattern matching: %s\n" (match 1::[] with
		| h::[] when h = 1 -> "only [1]"
		| h::t when h = 1 || t = [1] -> "[1, 1]"
		| _ -> "list"
	);

	Printf.printf "test pattern matching: %s\n" (match 1::[] with
		| h::[] when h = 2 -> "only [1]"
		| h::t when h = 1 || t = [5] -> "[1, 1]"
		| _ -> "list"
	);

	Printf.printf "test Left Right: %d\n" (double_right (Right 2));
	print_my_list test_lst;

	let zero  = Zero in
	let one   = Succ zero in

	let iszero (n : nat) : bool =
    match n with
      | Zero   -> true
      | Succ m -> false
  in

  let pred (n : nat) : nat =
    match n with
      | Zero   -> failwith "pred Zero is undefined"
      | Succ m -> m
 	in

	Printf.printf "Succ: %b\n" (iszero one);
	Printf.printf "Pred: %b\n" (one |> pred |> iszero);

	let map_f = function
	  | A x -> A (x + 1)
	  | B x -> B (x ^ "test")
	  | C x -> C (not x)
	in
	map map_f test_lst |> print_my_list;

	Printf.printf "And: %b\n" (and_rec [true; true; false]);

type transition = {
	read: string;
	to_state: string;
	write: string;
	action: string;
}

type dict_comp = {
	name: string;
	alphabet: string list;
	blank: string;
	states: string list;
	initial: string;
	final: string list;
	transitions: {name: string; data: transition list} list;
}


