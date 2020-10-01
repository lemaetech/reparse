module M = Make_test

let char_h (type s) (module P : M.S with type src = s) src () =
  let p = P.map2 (fun c o -> (c, o)) (P.char 'h') P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair char int) "'h', 1" ('h', 1) r)

let char_exn (type s) (module P : M.S with type src = s) src () =
  let p = P.char 'h' in
  let r () = ignore (P.parse src p) in
  Alcotest.(
    check_raises
      "char"
      (P.Parse_error
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[char] expected 'h'" })
      r)

let string (type s) (module P : M.S with type src = s) src () =
  let p = P.map2 (fun s o -> (s, o)) (P.string "hello") P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair string int) "\"hello\", 4" ("hello", 5) r)

let string_exn (type s) (module P : M.S with type src = s) src () =
  let p = P.string "hello" in
  let r () = ignore (P.parse src p) in
  Alcotest.(
    check_raises
      "string"
      (P.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[string]"})
      r)

let is_char = function
  | 'a'
  | 'b'
  | 'c' ->
      true
  | _ -> false

let satisfy (type s) (module P : M.S with type src = s) src () =
  let p = P.satisfy is_char in
  let r = P.parse src p in
  Alcotest.(check char "a" 'a' r)

let satisfy_exn (type s) (module P : M.S with type src = s) src () =
  let p = P.satisfy is_char in
  let r () = ignore (P.parse src p) in
  Alcotest.(
    check_raises
      "satisfy"
      (P.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[satisfy]"})
      r)

let line_lf (type s) (module P : M.S with type src = s) src () =
  let p = P.take (P.line `LF) in
  let r = P.parse src p in
  Alcotest.(
    check (pair int (list string)) "3, lines" (3, ["abc"; "def"; "ghi"]) r)

let line_crlf (type s) (module P : M.S with type src = s) src () =
  let p = P.take (P.line `CRLF) in
  let r = P.parse src p in
  Alcotest.(
    check (pair int (list string)) "3, lines" (3, ["abc"; "def"; "ghi"]) r)

let suite =
  [ M.make_string_test "char " char_h "hello"
  ; M.make_string_test "char exn" char_exn "aaaa"
  ; M.make_string_test "string" string "hello"
  ; M.make_string_test "string exn" string_exn "world"
  ; M.make_string_test "satisfy" satisfy "a"
  ; M.make_string_test "satisfy exn" satisfy_exn "d"
  ; M.make_string_test "line lf" line_lf "abc\ndef\nghi"
  ; M.make_string_test "line crlf" line_crlf "abc\r\ndef\r\nghi"
  ; M.make_file_test "char " char_h "hello"
  ; M.make_file_test "char exn" char_exn "aaaa"
  ; M.make_file_test "string" string "hello"
  ; M.make_file_test "string exn" string_exn "world"
  ; M.make_file_test "satisfy" satisfy "a"
  ; M.make_file_test "satisfy exn" satisfy_exn "d"
  ; M.make_file_test "line lf" line_lf "abc\ndef\nghi"
  ; M.make_file_test "line crlf" line_crlf "abc\r\ndef\r\nghi" ]
