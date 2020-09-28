module P = Reparse.Parse.String_parser
open P.Infix

let src = Reparse.IO.String.create

let peek_char_h () =
  let p = P.peek_char in
  let r = P.parse (src "hello") p in
  Alcotest.(check char "'h'" 'h' r)

let peek_char_offset () =
  let p = P.peek_char *> P.offset in
  let r = P.parse (src "hello") p in
  Alcotest.(check int "0" 0 r)

let peek_char_many () =
  let p = P.peek_char *> P.peek_char *> P.offset in
  let r = P.parse (src "hello") p in
  Alcotest.(check int "0" 0 r)

let peek_string_5 () =
  let r = P.parse (src "hello") (P.peek_string 5) in
  Alcotest.(check string "hello" "hello" r)

let peek_char_exn () =
  let p () = ignore (P.parse (src "") P.peek_char) in
  Alcotest.(
    check_raises
      "peek_char"
      (P.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[peek_char]"})
      p)

let peek_string_exn () =
  let p () = ignore (P.parse (src "hello") (P.peek_string 6)) in
  Alcotest.(
    check_raises
      "peek_string 6"
      (P.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[peek_string]"})
      p)

let next () =
  let p = P.map2 (fun c o -> (c, o)) P.next P.offset in
  let r = P.parse (src "hello") p in
  Alcotest.(check (pair char int) "'h',1" ('h', 1) r)

let next_exn () =
  let p1 = P.next *> P.next *> P.next in
  let p () = ignore (P.parse (src "hh") p1) in
  Alcotest.(
    check_raises
      "next exn"
      (P.Parse_error
         {offset = 2; line_number = 0; column_number = 0; msg = "[next]"})
      p)

let return_int () =
  let r = P.parse (src "") (P.return 5) in
  Alcotest.(check int "5" 5 r)

let return_string () =
  let r = P.parse (src "") (P.return "hello") in
  Alcotest.(check string "hello" "hello" r)

let optional () =
  let p = P.optional (P.char 'h') in
  let r = P.parse (src "hello") p in
  Alcotest.(check (option char) "'h'" (Some 'h') r)

let optional_none () =
  let p = P.optional (P.char 'h') in
  let r = P.parse (src "world") p in
  Alcotest.(check (option char) "'h'" None r)

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
