open Reparse

let test_return_int () =
  let r = parse "" (return 5) in
  Alcotest.(check int "5" 5 r)

let test_return_string () =
  let r = parse "" (return "hello") in
  Alcotest.(check string "hello" "hello" r)

let suite =
  [ ("return : 5", `Quick, test_return_int)
  ; ("return : \"hello\"", `Quick, test_return_string) ]
