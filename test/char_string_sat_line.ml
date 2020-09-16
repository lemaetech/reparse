module R = Reparse

let char_h () =
  let p = R.map2 (fun c o -> (c, o)) (R.char 'h') R.offset in
  let r = R.parse "hello" p in
  Alcotest.(check (pair unit int) "'h', 1" ((), 1) r)

let char_exn () =
  let p = R.char 'h' in
  let r () = ignore (R.parse "aaaa" p) in
  Alcotest.(
    check_raises
      "char"
      (R.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[char]"})
      r)

let string () =
  let p = R.map2 (fun s o -> (s, o)) (R.string "hello") R.offset in
  let r = R.parse "hello" p in
  Alcotest.(check (pair unit int) "\"hello\", 4" ((), 5) r)

let string_exn () =
  let p = R.string "hello" in
  let r () = ignore (R.parse "world" p) in
  Alcotest.(
    check_raises
      "string"
      (R.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[string]"})
      r)

let is_char = function
  | 'a'
  | 'b'
  | 'c' ->
      true
  | _ -> false

let satisfy () =
  let p = R.satisfy is_char in
  let r = R.parse "a" p in
  Alcotest.(check char "a" 'a' r)

let satisfy_exn () =
  let p = R.satisfy is_char in
  let r () = ignore (R.parse "d" p) in
  Alcotest.(
    check_raises
      "satisfy"
      (R.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[satisfy]"})
      r)

let suite =
  [ ("char ", `Quick, char_h)
  ; ("char exn", `Quick, char_exn)
  ; ("string", `Quick, string)
  ; ("string exn", `Quick, string_exn)
  ; ("satisfy", `Quick, satisfy)
  ; ("satisfy exn", `Quick, satisfy_exn) ]
