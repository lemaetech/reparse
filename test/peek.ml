open Reparse.Infix
module R = Reparse

let peek_char_h () =
  let p = R.peek_char in
  let r = R.parse "hello" p in
  Alcotest.(check char "'h'" 'h' r)

let peek_char_offset () =
  let p = R.peek_char *> R.offset in
  let r = R.parse "hello" p in
  Alcotest.(check int "0" 0 r)

let peek_char_many () =
  let p = R.peek_char *> R.peek_char *> R.offset in
  let r = R.parse "hello" p in
  Alcotest.(check int "0" 0 r)

let peek_string_5 () =
  let r = R.parse "hello" (R.peek_string 5) in
  Alcotest.(check string "hello" "hello" r)

let peek_char_exn () =
  let p () = ignore (R.parse "" R.peek_char) in
  Alcotest.(
    check_raises
      "peek_char"
      (R.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[peek_char]"})
      p)

let peek_string_exn () =
  let p () = ignore (R.parse "hello" (R.peek_string 6)) in
  Alcotest.(
    check_raises
      "peek_string 6"
      (R.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[peek_string]"})
      p)

let suite =
  [ ("char : 'h'", `Quick, peek_char_h)
  ; ("offset :0 0", `Quick, peek_char_offset)
  ; ("many/offset : 0", `Quick, peek_char_many)
  ; ("peek_string 5", `Quick, peek_string_5)
  ; ("peek_string exn", `Quick, peek_string_exn)
  ; ("peek_char exn", `Quick, peek_char_exn) ]
