(* Riconosce il linguaggio [01]+ *)
let lang1 w =
  (* Una lista è valida se contiene solo '0' e '1' ed è non vuota *)
  List.length w > 0 && List.for_all (fun c -> c = '0' || c = '1') w

(* Riconosce il linguaggio 0?1* *)
let lang2 w =
  match w with
  | [] -> true (* Una lista vuota appartiene al linguaggio *)
  | '0' :: rest -> List.for_all ((=) '1') rest
  | _ -> List.for_all ((=) '1') w

(* Riconosce il linguaggio 0[01]*0 *)
let lang3 w =
  match w with
  | '0' :: rest ->
      (match List.rev rest with
      | '0' :: middle -> List.for_all (fun c -> c = '0' || c = '1') middle
      | _ -> false)
  | _ -> false

(* Riconosce il linguaggio 0*10*10* *)
let lang4 w =
  let rec aux seen_one = function
    | [] -> false
    | '1' :: rest -> if seen_one then true else aux true rest
    | '0' :: rest -> aux seen_one rest
    | _ -> false
  in
  aux false w

(* Riconosce il linguaggio (00|11)+ *)
let lang5 w =
  let rec aux = function
    | [] -> false
    | '0' :: '0' :: rest -> (rest = [] || aux rest)
    | '1' :: '1' :: rest -> (rest = [] || aux rest)
    | _ -> false
  in
  aux w
    
let recognizers = [lang1;lang2;lang3;lang4;lang5]
                  
let belongsTo w = List.map (fun f -> f w) recognizers
  
