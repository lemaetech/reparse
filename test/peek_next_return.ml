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

let next () =
  let p = R.map2 (fun c o -> (c, o)) R.next R.offset in
  let r = R.parse "hello" p in
  Alcotest.(check (pair char int) "'h',1" ('h', 1) r)

let next_exn () =
  let p1 = R.next *> R.next *> R.next in
  let p () = ignore (R.parse "hh" p1) in
  Alcotest.(
    check_raises
      "next exn"
      (R.Parse_error
         {offset = 2; line_number = 0; column_number = 0; msg = "[next]"})
      p)

let return_int () =
  let r = R.parse "" (R.return 5) in
  Alcotest.(check int "5" 5 r)

let return_string () =
  let r = R.parse "" (R.return "hello") in
  Alcotest.(check string "hello" "hello" r)

let optional () =
  let p = R.optional (R.char 'h') in
  let r = R.parse "hello" p in
  Alcotest.(check (option unit) "'h'" (Some ()) r)

let optional_none () =
  let p = R.optional (R.char 'h') in
  let r = R.parse "world" p in
  Alcotest.(check (option unit) "'h'" None r)

let suite =
  [ ("char : 'h'", `Quick, peek_char_h)
  ; ("offset :0 0", `Quick, peek_char_offset)
  ; ("many/offset : 0", `Quick, peek_char_many)
  ; ("peek_string 5", `Quick, peek_string_5)
  ; ("peek_string exn", `Quick, peek_string_exn)
  ; ("peek_char exn", `Quick, peek_char_exn)
  ; ("next", `Quick, next)
  ; ("next exn", `Quick, next_exn)
  ; ("return : 5", `Quick, return_int)
  ; ("return : \"hello\"", `Quick, return_string)
  ; ("optional Some", `Quick, optional)
  ; ("optional None", `Quick, optional_none) ]
