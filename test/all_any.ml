module P = Reparse.Parser

let any input () =
  let p = P.any [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r = P.parse input p in
  Alcotest.(check char "c" 'c' r)

let any_fail input () =
  let p = P.any [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r () = ignore (P.parse input p) in
  Alcotest.(
    check_raises
      "any"
      (P.Parser
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[any] all parsers failed" })
      r)

let all input () =
  let p = P.all [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r = P.parse input p in
  Alcotest.(check (list char) "list" ['a'; 'b'; 'c'] r)

let all_fail input () =
  let p = P.all [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r () = ignore (P.parse input p) in
  Alcotest.(
    check_raises
      "all fail"
      (P.Parser
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[all] one of the parsers failed" })
      r)

let all_unit input () =
  let p = P.all_unit [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r = P.parse input p in
  Alcotest.(check unit "()" () r)

let all_unit_fail input () =
  let p = P.all_unit [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r () = ignore (P.parse input p) in
  Alcotest.(
    check_raises
      "all fail"
      (P.Parser
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[all_unit] one of the parsers failed" })
      r)

module M = Make_test

let suite =
  [ M.make "any" any "cabd"
  ; M.make "any fail" any_fail "zzz"
  ; M.make "all" all "abcd"
  ; M.make "all fail" all_fail "abz"
  ; M.make "all_unit" all_unit "abcd"
  ; M.make "all_unit fail" all_unit_fail "abz" ]
  |> List.concat
