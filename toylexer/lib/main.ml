open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

  (* take : int -> 'a list -> 'a list *)
let rec take n lst =
  match (n, lst) with
  | (0, _) | (_, []) -> []
  | (n, x :: xs) -> x :: take (n - 1) xs

(* frequency : int -> 'a list -> ('a * int) list *)
let frequency n lst =
  (* Conta le occorrenze di ciascun elemento nella lista *)
  let counts = List.fold_left (fun acc x ->
    match List.assoc_opt x acc with
    | Some count -> (x, count + 1) :: List.remove_assoc x acc
    | None -> (x, 1) :: acc
  ) [] lst in
  (* Ordina gli elementi per numero di occorrenze decrescente *)
  let sorted = List.sort (fun (_, count1) (_, count2) -> compare count2 count1) counts in
  (* Prendi solo i primi n elementi *)
  take n sorted

