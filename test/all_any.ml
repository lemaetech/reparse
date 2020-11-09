module M = Make_test

let any (type s) (module P : M.S with type src = s) src () =
  let p = P.any [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r = P.parse src p in
  Alcotest.(check char "c" 'c' r)

let any_fail (type s) (module P : M.S with type src = s) src () =
  let p = P.any [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r () = ignore (P.parse src p) in
  Alcotest.(
    check_raises
      "any"
      (P.Parse_error
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[any] all parsers failed" })
      r)

let all (type s) (module P : M.S with type src = s) src () =
  let p = P.all [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r = P.parse src p in
  Alcotest.(check (list char) "list" ['a'; 'b'; 'c'] r)

let all_fail (type s) (module P : M.S with type src = s) src () =
  let p = P.all [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r () = ignore (P.parse src p) in
  Alcotest.(
    check_raises
      "all fail"
      (P.Parse_error
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[all] one of the parsers failed" })
      r)

let all_unit (type s) (module P : M.S with type src = s) src () =
  let p = P.all_unit [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r = P.parse src p in
  Alcotest.(check unit "()" () r)

let all_unit_fail (type s) (module P : M.S with type src = s) src () =
  let p = P.all_unit [P.char 'a'; P.char 'b'; P.char 'c'] in
  let r () = ignore (P.parse src p) in
  Alcotest.(
    check_raises
      "all fail"
      (P.Parse_error
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[all_unit] one of the parsers failed" })
      r)

let suite =
  [ M.make_string_test "any" any "cabd"
  ; M.make_string_test "any fail" any_fail "zzz"
  ; M.make_string_test "all" all "abcd"
  ; M.make_string_test "all fail" all_fail "abz"
  ; M.make_string_test "all_unit" all_unit "abcd"
  ; M.make_string_test "all_unit fail" all_unit_fail "abz"
  ; M.make_file_test "any" any "cabd"
  ; M.make_file_test "any fail" any_fail "zzz"
  ; M.make_file_test "all" all "abcd"
  ; M.make_file_test "all fail" all_fail "abz"
  ; M.make_file_test "all_unit" all_unit "abcd"
  ; M.make_file_test "all_unit fail" all_unit_fail "abz" ]
