module M = Make_test

let make_pair a b = (a, b)

let skip (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_pair (P.skip P.space) P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair int int) "4, 4" (4, 4) r)

let skip_at_least (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_pair (P.skip ~at_least:3 P.space) P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair int int) "4, 4" (4, 4) r)

let skip_at_least_fail (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_pair (P.skip ~at_least:5 P.space) P.offset in
  let r () = ignore (P.parse src p) in
  Alcotest.(
    check_raises
      "skip ~at_least fails"
      (P.Parse_error
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[skip] unable to parse at_least 5 times" })
      r)

let skip_upto (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_pair (P.skip ~up_to:3 P.space) P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair int int) "3, 3" (3, 3) r)

let skip_skip_skip (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_pair (P.skip (P.skip (P.skip P.space))) P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair int int) "1, 5" (1, 5) r)

let skip_while (type s) (module P : M.S with type io = s) src () =
  let p =
    P.map2
      make_pair
      (P.skip_while P.next ~while_:(P.is_not (P.char 'z')))
      P.peek_char
  in
  let r = P.parse src p in
  Alcotest.(check (pair int char) "4, z" (4, 'z') r)

let skip_while2 (type s) (module P : M.S with type io = s) src () =
  let p =
    P.map2
      make_pair
      (P.skip_while (P.char 'a') ~while_:(P.is_not (P.char 'z')))
      P.peek_char
  in
  let r = P.parse src p in
  Alcotest.(check (pair int char) "4, c" (4, 'c') r)

let take (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_pair (P.take (P.char 'a')) P.offset in
  let r = P.parse src p in
  Alcotest.(
    check (pair (pair int (list char)) int) "" ((4, ['a'; 'a'; 'a'; 'a']), 4) r)

let take_at_least (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_pair (P.take ~at_least:4 (P.char 'a')) P.offset in
  let r = P.parse src p in
  Alcotest.(
    check (pair (pair int (list char)) int) "" ((4, ['a'; 'a'; 'a'; 'a']), 4) r)

let take_at_least_fail (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_pair (P.take ~at_least:5 (P.char 'a')) P.offset in
  let r () = ignore (P.parse src p) in
  Alcotest.(
    check_raises
      "skip ~at_least fails"
      (P.Parse_error
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[take] unable to parse at least 5 times" })
      r)

let take_up_to (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_pair (P.take ~up_to:3 (P.char 'a')) P.offset in
  let r = P.parse src p in
  Alcotest.(
    check (pair (pair int (list char)) int) "" ((3, ['a'; 'a'; 'a']), 3) r)

let take_sep_by (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_pair (P.take ~sep_by:P.space (P.char 'a')) P.offset in
  let r = P.parse src p in
  Alcotest.(
    check (pair (pair int (list char)) int) "" ((4, ['a'; 'a'; 'a'; 'a']), 7) r)

let take_at_least_up_to_sep_by (type s) (module P : M.S with type io = s) src ()
    =
  let p =
    P.map2
      make_pair
      (P.take ~at_least:3 ~up_to:3 ~sep_by:P.space (P.char 'a'))
      P.offset
  in
  let r = P.parse src p in
  Alcotest.(
    check (pair (pair int (list char)) int) "" ((3, ['a'; 'a'; 'a']), 6) r)

let suite =
  [ M.make_string_test "skip" skip "    a"
  ; M.make_string_test "skip ~at_least:3" skip_at_least "    a"
  ; M.make_string_test "skip ~at_least:5 fails" skip_at_least_fail "    a"
  ; M.make_string_test "skip ~up_to:3" skip_upto "     a"
  ; M.make_string_test "skip skip skip" skip_skip_skip "     a"
  ; M.make_string_test "skip while" skip_while "aaaaz"
  ; M.make_string_test "skip while" skip_while2 "aaaacz"
  ; M.make_string_test "take" take "aaaacz"
  ; M.make_string_test "take at least" take_at_least "aaaacz"
  ; M.make_string_test "take at least fail" take_at_least_fail "aaaacz"
  ; M.make_string_test "take up_to" take_up_to "aaaacz"
  ; M.make_string_test "take sep_by" take_sep_by "a a a acz"
  ; M.make_string_test
      "take at_least up_to sep_by"
      take_at_least_up_to_sep_by
      "a a a acz"
  ; M.make_file_test "skip" skip "    a"
  ; M.make_file_test "skip ~at_least:3" skip_at_least "    a"
  ; M.make_file_test "skip ~at_least:5 fails" skip_at_least_fail "    a"
  ; M.make_file_test "skip ~up_to:3" skip_upto "     a"
  ; M.make_file_test "skip skip skip" skip_skip_skip "     a"
  ; M.make_file_test "skip while" skip_while "aaaaz"
  ; M.make_file_test "skip while" skip_while2 "aaaacz"
  ; M.make_file_test "take" take "aaaacz"
  ; M.make_file_test "take at least" take_at_least "aaaacz"
  ; M.make_file_test "take at least fail" take_at_least_fail "aaaacz"
  ; M.make_file_test "take up_to" take_up_to "aaaacz"
  ; M.make_file_test "take sep_by" take_sep_by "a a a acz"
  ; M.make_file_test
      "take at_least up_to sep_by"
      take_at_least_up_to_sep_by
      "a a a acz" ]
