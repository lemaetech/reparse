module M = Make_test

let peek_char_h (type s) (module P : M.S with type input = s) input () =
  let p = P.peek_char in
  let r = P.parse input p in
  Alcotest.(check char "'h'" 'h' r)

let peek_char_offset (type s) (module P : M.S with type input = s) input () =
  let open P.Infix in
  let p = P.peek_char *> P.offset in
  let r = P.parse input p in
  Alcotest.(check int "0" 0 r)

let peek_char_many (type s) (module P : M.S with type input = s) input () =
  let open P.Infix in
  let p = P.peek_char *> P.peek_char *> P.offset in
  let r = P.parse input p in
  Alcotest.(check int "0" 0 r)

let peek_string_5 (type s) (module P : M.S with type input = s) input () =
  let r = P.parse input (P.peek_string 5) in
  Alcotest.(check string "hello" "hello" r)

let peek_char_exn (type s) (module P : M.S with type input = s) input () =
  let p () = ignore (P.parse input P.peek_char) in
  Alcotest.(
    check_raises
      "peek_char"
      (P.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[peek_char]"})
      p)

let peek_string_exn (type s) (module P : M.S with type input = s) input () =
  let p () = ignore (P.parse input (P.peek_string 6)) in
  Alcotest.(
    check_raises
      "peek_string 6"
      (P.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[peek_string]"})
      p)

let next (type s) (module P : M.S with type input = s) input () =
  let p = P.map2 (fun c o -> (c, o)) P.next P.offset in
  let r = P.parse input p in
  Alcotest.(check (pair char int) "'h',1" ('h', 1) r)

let next_exn (type s) (module P : M.S with type input = s) input () =
  let open P.Infix in
  let p1 = P.next *> P.next *> P.next in
  let p () = ignore (P.parse input p1) in
  Alcotest.(
    check_raises
      "next exn"
      (P.Parse_error
         {offset = 2; line_number = 0; column_number = 0; msg = "[next]"})
      p)

let return_int (type s) (module P : M.S with type input = s) input () =
  let r = P.parse input (P.return 5) in
  Alcotest.(check int "5" 5 r)

let return_string (type s) (module P : M.S with type input = s) input () =
  let r = P.parse input (P.return "hello") in
  Alcotest.(check string "hello" "hello" r)

let optional (type s) (module P : M.S with type input = s) input () =
  let p = P.optional (P.char 'h') in
  let r = P.parse input p in
  Alcotest.(check (option char) "'h'" (Some 'h') r)

let optional_none (type s) (module P : M.S with type input = s) input () =
  let p = P.optional (P.char 'h') in
  let r = P.parse input p in
  Alcotest.(check (option char) "'h'" None r)

let suite =
  [ M.make_string_test "char : 'h'" peek_char_h "hello"
  ; M.make_string_test "offset :0 0" peek_char_offset "hello"
  ; M.make_string_test "many/offset : 0" peek_char_many "hello"
  ; M.make_string_test "peek_string 5" peek_string_5 "hello"
  ; M.make_string_test "peek_string exn" peek_string_exn "hello"
  ; M.make_string_test "peek_char exn" peek_char_exn ""
  ; M.make_string_test "next" next "hello"
  ; M.make_string_test "next exn" next_exn "hh"
  ; M.make_string_test "return : 5" return_int ""
  ; M.make_string_test "return : \"hello\"" return_string ""
  ; M.make_string_test "optional Some" optional "hello"
  ; M.make_string_test "optional None" optional_none "world"
  ; M.make_file_test "char : 'h'" peek_char_h "hello"
  ; M.make_file_test "offset :0 0" peek_char_offset "hello"
  ; M.make_file_test "many/offset : 0" peek_char_many "hello"
  ; M.make_file_test "peek_string 5" peek_string_5 "hello"
  ; M.make_file_test "peek_string exn" peek_string_exn "hello"
  ; M.make_file_test "peek_char exn" peek_char_exn ""
  ; M.make_file_test "next" next "hello"
  ; M.make_file_test "next exn" next_exn "hh"
  ; M.make_file_test "return : 5" return_int ""
  ; M.make_file_test "return : \"hello\"" return_string ""
  ; M.make_file_test "optional Some" optional "hello"
  ; M.make_file_test "optional None" optional_none "world" ]
