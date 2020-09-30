module type S = Reparse.Parse_sig.S

type 's test =
  (module Reparse.Parse_sig.S with type io = 's) -> 's -> unit -> unit

let peek_char_h (type s) (module P : S with type io = s) src () =
  let p = P.peek_char in
  let r = P.parse src p in
  Alcotest.(check char "'h'" 'h' r)

let peek_char_offset (type s) (module P : S with type io = s) src () =
  let open P.Infix in
  let p = P.peek_char *> P.offset in
  let r = P.parse src p in
  Alcotest.(check int "0" 0 r)

let peek_char_many (type s) (module P : S with type io = s) src () =
  let open P.Infix in
  let p = P.peek_char *> P.peek_char *> P.offset in
  let r = P.parse src p in
  Alcotest.(check int "0" 0 r)

let peek_string_5 (type s) (module P : S with type io = s) src () =
  let r = P.parse src (P.peek_string 5) in
  Alcotest.(check string "hello" "hello" r)

let peek_char_exn (type s) (module P : S with type io = s) src () =
  let p () = ignore (P.parse src P.peek_char) in
  Alcotest.(
    check_raises
      "peek_char"
      (P.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[peek_char]"})
      p)

let peek_string_exn (type s) (module P : S with type io = s) src () =
  let p () = ignore (P.parse src (P.peek_string 6)) in
  Alcotest.(
    check_raises
      "peek_string 6"
      (P.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[peek_string]"})
      p)

let next (type s) (module P : S with type io = s) src () =
  let p = P.map2 (fun c o -> (c, o)) P.next P.offset in
  let r = P.parse src p in
  Alcotest.(check (pair char int) "'h',1" ('h', 1) r)

let next_exn (type s) (module P : S with type io = s) src () =
  let open P.Infix in
  let p1 = P.next *> P.next *> P.next in
  let p () = ignore (P.parse src p1) in
  Alcotest.(
    check_raises
      "next exn"
      (P.Parse_error
         {offset = 2; line_number = 0; column_number = 0; msg = "[next]"})
      p)

let return_int (type s) (module P : S with type io = s) src () =
  let r = P.parse src (P.return 5) in
  Alcotest.(check int "5" 5 r)

let return_string (type s) (module P : S with type io = s) src () =
  let r = P.parse src (P.return "hello") in
  Alcotest.(check string "hello" "hello" r)

let optional (type s) (module P : S with type io = s) src () =
  let p = P.optional (P.char 'h') in
  let r = P.parse src p in
  Alcotest.(check (option char) "'h'" (Some 'h') r)

let optional_none (type s) (module P : S with type io = s) src () =
  let p = P.optional (P.char 'h') in
  let r = P.parse src p in
  Alcotest.(check (option char) "'h'" None r)

let () = Random.self_init ()

let make_file content =
  let fname = string_of_int @@ Random.bits () in
  let fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT] 0o640 in
  let _w = Unix.write_substring fd content 0 (String.length content) in
  fd

let make_string_test test_name (test : 's test) test_data =
  let src = Reparse.Io.String.create test_data in
  ( "[String] " ^ test_name
  , `Quick
  , test (module Reparse.Parse.String_parser) src )

let make_file_test test_name (test : 's test) test_data =
  let src = make_file test_data |> Reparse.Io.File.create in
  ("[File]   " ^ test_name, `Quick, test (module Reparse.Parse.File_parser) src)

let suite =
  [ make_string_test "char : 'h'" peek_char_h "hello"
  ; make_string_test "offset :0 0" peek_char_offset "hello"
  ; make_string_test "many/offset : 0" peek_char_many "hello"
  ; make_string_test "peek_string 5" peek_string_5 "hello"
  ; make_string_test "peek_string exn" peek_string_exn "hello"
  ; make_string_test "peek_char exn" peek_char_exn ""
  ; make_string_test "next" next "hello"
  ; make_string_test "next exn" next_exn "hh"
  ; make_string_test "return : 5" return_int ""
  ; make_string_test "return : \"hello\"" return_string ""
  ; make_string_test "optional Some" optional "hello"
  ; make_string_test "optional None" optional_none "world"
  ; make_file_test "char : 'h'" peek_char_h "hello"
  ; make_file_test "offset :0 0" peek_char_offset "hello"
  ; make_file_test "many/offset : 0" peek_char_many "hello"
  ; make_file_test "peek_string 5" peek_string_5 "hello"
  ; make_file_test "peek_string exn" peek_string_exn "hello"
  ; make_file_test "peek_char exn" peek_char_exn ""
  ; make_file_test "next" next "hello"
  ; make_file_test "next exn" next_exn "hh"
  ; make_file_test "return : 5" return_int ""
  ; make_file_test "return : \"hello\"" return_string ""
  ; make_file_test "optional Some" optional "hello"
  ; make_file_test "optional None" optional_none "world" ]
