module P = Reparse.Parser

let peek_char_h parse () =
  let p = P.peek_char in
  let r = parse p in
  Alcotest.(check char "'h'" 'h' r)

let peek_char_offset parse () =
  let open P.Infix in
  let p = P.peek_char *> P.offset in
  let r = parse p in
  Alcotest.(check int "0" 0 r)

let peek_char_many parse () =
  let open P.Infix in
  let p = P.peek_char *> P.peek_char *> P.offset in
  let r = parse p in
  Alcotest.(check int "0" 0 r)

let peek_string_5 parse () =
  let r = parse (P.peek_string 5) in
  Alcotest.(check string "hello" "hello" r)

let peek_char_exn parse () =
  let p () = ignore (parse P.peek_char) in
  Alcotest.(
    check_raises "peek_char"
      (P.Parser {offset= 0; line_number= 0; column_number= 0; msg= "[peek_char]"}
      )
      p)

let peek_string_exn parse () =
  let p () = ignore (parse (P.peek_string 6)) in
  Alcotest.(
    check_raises "peek_string 6"
      (P.Parser
         {offset= 0; line_number= 0; column_number= 0; msg= "[peek_string]"} )
      p)

let next parse () =
  let p = P.map2 (fun c o -> (c, o)) P.next P.offset in
  let r = parse p in
  Alcotest.(check (pair char int) "'h',1" ('h', 1) r)

let next_exn parse () =
  let open P.Infix in
  let p1 = P.next *> P.next *> P.next in
  let p () = ignore (parse p1) in
  Alcotest.(
    check_raises "next exn"
      (P.Parser {offset= 2; line_number= 0; column_number= 0; msg= "[next]"})
      p)

let pure_int parse () =
  let r = parse (P.pure 5) in
  Alcotest.(check int "5" 5 r)

let pure_string parse () =
  let r = parse (P.pure "hello") in
  Alcotest.(check string "hello" "hello" r)

let optional parse () =
  let p = P.optional (P.char 'h') in
  let r = parse p in
  Alcotest.(check (option char) "'h'" (Some 'h') r)

let optional_none parse () =
  let p = P.optional (P.char 'h') in
  let r = parse p in
  Alcotest.(check (option char) "'h'" None r)

module M = Make_test

let suite =
  [ M.make "char : 'h'" peek_char_h "hello"
  ; M.make "offset :0 0" peek_char_offset "hello"
  ; M.make "many/offset : 0" peek_char_many "hello"
  ; M.make "peek_string 5" peek_string_5 "hello"
  ; M.make "peek_string exn" peek_string_exn "hello"
  ; M.make "peek_char exn" peek_char_exn ""; M.make "next" next "hello"
  ; M.make "next exn" next_exn "hh"; M.make "pure : 5" pure_int ""
  ; M.make "pure : \"hello\"" pure_string ""
  ; M.make "optional Some" optional "hello"
  ; M.make "optional None" optional_none "world" ]
  |> List.concat
