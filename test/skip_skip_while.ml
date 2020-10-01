module M = Make_test

let make_tuple a b = (a, b)

let skip (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_tuple (P.skip P.space) P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair int int) "4, 4" (4, 4) r)

let skip_at_least (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_tuple (P.skip ~at_least:3 P.space) P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair int int) "4, 4" (4, 4) r)

let skip_at_least_fail (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_tuple (P.skip ~at_least:5 P.space) P.offset in
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
  let p = P.map2 make_tuple (P.skip ~up_to:3 P.space) P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair int int) "3, 3" (3, 3) r)

let skip_skip_skip (type s) (module P : M.S with type io = s) src () =
  let p = P.map2 make_tuple (P.skip (P.skip (P.skip P.space))) P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair int int) "1, 5" (1, 5) r)

let suite =
  [ M.make_string_test "skip" skip "    a"
  ; M.make_string_test "skip ~at_least:3" skip_at_least "    a"
  ; M.make_string_test "skip ~at_least:5 fails" skip_at_least_fail "    a"
  ; M.make_string_test "skip ~up_to:3" skip_upto "     a"
  ; M.make_string_test "skip skip skip" skip_skip_skip "     a"
  ; M.make_file_test "skip" skip "    a"
  ; M.make_file_test "skip ~at_least:3" skip_at_least "    a"
  ; M.make_file_test "skip ~at_least:5 fails" skip_at_least_fail "    a"
  ; M.make_file_test "skip ~up_to:3" skip_upto "     a"
  ; M.make_file_test "skip skip skip" skip_skip_skip "     a" ]
