open Tugofwar

(* Importa le funzioni da tugofwar.ml *)
let%test _ = toklist_of_string "ABX" = ['A'; 'B'; 'X']
let%test _ = toklist_of_string "ABXZ" = []
let%test _ = toklist_of_string "" = []

(*Test valid*)
let%test _ = valid ['A'; 'A'; 'B'; 'B'] = true
let%test _ = valid ['A'; 'A'; 'B'] = false
let%test _ = valid [] = true

(*test win*)
let%test _ = win ['A'; 'A'; 'B'] = 'A'
let%test _ = win ['A'; 'B'; 'B'] = 'B'
let%test _ = win ['A'; 'B'] = 'X'

(*
let%test_win _ =
  assert_equal 'A' (win ['A'; 'A'; 'B']);
  assert_equal 'B' (win ['A'; 'B'; 'B']);
  assert_equal 'X' (win ['A'; 'B'])

let%test_string_of_winner _ =
  assert_equal "Team A wins!" (string_of_winner 'A');
  assert_equal "Team B wins!" (string_of_winner 'B');
  assert_equal "It's a tie!" (string_of_winner 'X')


(* Esegui i test *)
let () = run_test_tt_main suite*)

