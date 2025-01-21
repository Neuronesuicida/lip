open Recognizer

let%test "lang1" = lang1 ['0'; '1'; '1'] = true
let%test "lang1_empty" = lang1 [] = false
let%test "lang2" = lang2 ['0'; '1'; '1'] = true
let%test "lang3" = lang3 ['0'; '1'; '0'] = true
let%test "lang4" = lang4 ['0'; '1'; '0'; '1'] = true
let%test "lang5" = lang5 ['1'; '1'; '0'; '0'] = true
let%test "belongsTo_all" = belongsTo ['0'; '1'; '0'] = [true; false; true; false; false]
