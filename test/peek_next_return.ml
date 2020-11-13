module P = Reparse.Parser

let peek_char_h input () =
  let p = P.peek_char in
  let r = P.parse input p in
  Alcotest.(check char "'h'" 'h' r)

let peek_char_offset input () =
  let open P.Infix in
  let p = P.peek_char *> P.offset in
  let r = P.parse input p in
  Alcotest.(check int "0" 0 r)

let peek_char_many input () =
  let open P.Infix in
  let p = P.peek_char *> P.peek_char *> P.offset in
  let r = P.parse input p in
  Alcotest.(check int "0" 0 r)

let peek_string_5 input () =
  let r = P.parse input (P.peek_string 5) in
  Alcotest.(check string "hello" "hello" r)

let peek_char_exn input () =
  let p () = ignore (P.parse input P.peek_char) in
  Alcotest.(
    check_raises
      "peek_char"
      (P.Parser
         {offset = 0; line_number = 0; column_number = 0; msg = "[peek_char]"})
      p)

let peek_string_exn input () =
  let p () = ignore (P.parse input (P.peek_string 6)) in
  Alcotest.(
    check_raises
      "peek_string 6"
      (P.Parser
         {offset = 0; line_number = 0; column_number = 0; msg = "[peek_string]"})
      p)

let next input () =
  let p = P.map2 (fun c o -> (c, o)) P.next P.offset in
  let r = P.parse input p in
  Alcotest.(check (pair char int) "'h',1" ('h', 1) r)

let next_exn input () =
  let open P.Infix in
  let p1 = P.next *> P.next *> P.next in
  let p () = ignore (P.parse input p1) in
  Alcotest.(
    check_raises
      "next exn"
      (P.Parser {offset = 2; line_number = 0; column_number = 0; msg = "[next]"})
      p)

let return_int input () =
  let r = P.parse input (P.return 5) in
  Alcotest.(check int "5" 5 r)

let return_string input () =
  let r = P.parse input (P.return "hello") in
  Alcotest.(check string "hello" "hello" r)

let optional input () =
  let p = P.optional (P.char 'h') in
  let r = P.parse input p in
  Alcotest.(check (option char) "'h'" (Some 'h') r)

let optional_none input () =
  let p = P.optional (P.char 'h') in
  let r = P.parse input p in
  Alcotest.(check (option char) "'h'" None r)

module M = Make_test

let suite =
  [ M.make "char : 'h'" peek_char_h "hello"
  ; M.make "offset :0 0" peek_char_offset "hello"
  ; M.make "many/offset : 0" peek_char_many "hello"
  ; M.make "peek_string 5" peek_string_5 "hello"
  ; M.make "peek_string exn" peek_string_exn "hello"
  ; M.make "peek_char exn" peek_char_exn ""
  ; M.make "next" next "hello"
  ; M.make "next exn" next_exn "hh"
  ; M.make "return : 5" return_int ""
  ; M.make "return : \"hello\"" return_string ""
  ; M.make "optional Some" optional "hello"
  ; M.make "optional None" optional_none "world" ]
  |> List.concat
