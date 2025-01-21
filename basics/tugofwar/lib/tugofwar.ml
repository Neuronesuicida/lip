(* tokens *)
type token = A | B | X

(* val toklist_of_string : string -> token list *)
(* toklist_of_string s transforms the string s into a list of tokens *)
(* Hint: use the function explode in bin/main.ml to convert a string to a char list *)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []
             
let toklist_of_string s = 
  let char_to_token c =
    match c with
    | 'A' -> Some 'A'
    | 'B' -> Some 'B'
    | 'X' -> Some 'X'
    | _ -> None
  in
  let rec aux chars acc =
    match chars with
    | [] -> List.rev acc
    | h :: t ->
        match char_to_token h with
        | Some token -> aux t (token :: acc)
        | None -> []
  in
  aux (explode s) []

(* val valid : token list -> bool *)
(* valid l is true when l is a list of tokens in the language A* X* B* *)
    
let valid l = 
  let rec count_tokens tokens (count_a, count_b) =
    match tokens with
    | [] -> (count_a, count_b)
    | 'A' :: t -> count_tokens t (count_a + 1, count_b)
    | 'B' :: t -> count_tokens t (count_a, count_b + 1)
    | _ -> (count_a, count_b)
  in
  let count_a, count_b = count_tokens l (0, 0) in
  count_a = count_b

(* val win : token list -> token *)
(* win l determines the winner of a tug of war game. X means tie *)

let win l = 
  let rec score tokens (score_a, score_b) =
    match tokens with
    | [] -> (score_a, score_b)
    | 'A' :: t -> score t (score_a + 1, score_b)
    | 'B' :: t -> score t (score_a, score_b + 1)
    | _ -> (score_a, score_b)
  in
  let score_a, score_b = score l (0, 0) in
  if score_a > score_b then 'A'
  else if score_b > score_a then 'B'
  else 'X'

(* val string_of_winner : token -> string *)
let string_of_winner w = 
  match w with
  | 'A' -> "Team A wins!"
  | 'B' -> "Team B wins!"
  | 'X' -> "It's a tie!"
  | _ -> "Invalid token"
