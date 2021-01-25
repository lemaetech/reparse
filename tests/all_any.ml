open Reparse

let any parse () =
  let p = any [ char 'a'; char 'b'; char 'c' ] in
  let r = parse p in
  Alcotest.(check char "c" 'c' r)
;;

let any_fail parse () =
  let p = Reparse.(any [ char 'a'; char 'b'; char 'c' ]) in
  let r () = ignore (parse p) in
  Alcotest.(
    check_raises
      "any"
      (Parser
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[any] all parsers failed"
         })
      r)
;;

let all parse () =
  let p = all [ char 'a'; char 'b'; char 'c' ] in
  let r = parse p in
  Alcotest.(check (list char) "list" [ 'a'; 'b'; 'c' ] r)
;;

let all_fail parse () =
  let p = Reparse.(all [ char 'a'; char 'b'; char 'c' ]) in
  let r () = ignore (parse p) in
  Alcotest.(
    check_raises
      "all fail"
      (Parser
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[all] one of the parsers failed"
         })
      r)
;;

let char c = char c *> unit

let all_unit_test parse () =
  let p = all_unit [ char 'a'; char 'b'; char 'c' ] in
  let r = parse p in
  Alcotest.(check unit "()" () r)
;;

let all_unit_fail parse () =
  let p = all_unit [ char 'a'; char 'b'; char 'c' ] in
  let r () = ignore (parse p) in
  Alcotest.(
    check_raises
      "all fail"
      (Parser { offset = 2; line_number = 0; column_number = 0; msg = "[char] 'c'" })
      r)
;;

module M = Make_test

let suite =
  [ M.make "any" any "cabd"
  ; M.make "any fail" any_fail "zzz"
  ; M.make "all" all "abcd"
  ; M.make "all fail" all_fail "abz"
  ; M.make "all_unit" all_unit_test "abcd"
  ; M.make "all_unit fail" all_unit_fail "abz"
  ]
  |> List.concat
;;
