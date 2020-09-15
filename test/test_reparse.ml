open Reparse

let test_lnum_cnum_is_1_11 () =
  let p = many next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum in
  let r = parse ~track_lnum:true "hello world" p in
  Alcotest.(check (result (pair int int) string) "1/11" (Ok (1, 11)) r)

let test_lnum_cnum_is_3_0 () =
  let p = many next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum in
  let r = parse ~track_lnum:true "hello world\nsecond line\n" p in
  Alcotest.(check (result (pair int int) string) "3/0" (Ok (3, 0)) r)

let test_lnum_cnum_is_3_5 () =
  let p = many next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum in
  let r = parse ~track_lnum:true "hello world\nsecond line\naaaaa" p in
  Alcotest.(check (result (pair int int) string) "3/5" (Ok (3, 5)) r)

let test_lnum_cnum_is_0_0 () =
  let p = many next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum in
  let r = parse "hello world\nsecond line\naaaaa" p in

  Alcotest.(check (result (pair int int) string) "3/5" (Ok (0, 0)) r) ;

  let p = many next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum in
  let r = parse ~track_lnum:false "hello world\nsecond line\naaaaa" p in
  Alcotest.(check (result (pair int int) string) "3/5" (Ok (0, 0)) r)

let line_column_number_test_suite =
  [ ("lnum/cnum : 1/11", `Quick, test_lnum_cnum_is_1_11)
  ; ("lnum/cnum : 3/0", `Quick, test_lnum_cnum_is_3_0)
  ; ("lnum/cnum : 3/5", `Quick, test_lnum_cnum_is_3_5)
  ; ("lnum/cnum : 0/0", `Quick, test_lnum_cnum_is_0_0) ]

(* Peek tests. *)
let test_peek_char_h () =
  let p = peek_char in
  let r = parse "hello" p in
  Alcotest.(check (result char string) "'h'" (Ok 'h') r)

let test_peek_char_offset () =
  let p = peek_char *> offset in
  let r = parse "hello" p in
  Alcotest.(check (result int string) "0" (Ok 0) r)

let test_peek_char_many () =
  let p = peek_char *> peek_char *> offset in
  let r = parse "hello" p in
  Alcotest.(check (result int string) "0" (Ok 0) r)

let peek_char_test_suite =
  [ ("char : 'h'", `Quick, test_peek_char_h)
  ; ("offset :0 0", `Quick, test_peek_char_offset)
  ; ("many/offset : 0", `Quick, test_peek_char_many) ]

(* Next tests. *)
let test_next () =
  let p = map2 (fun c o -> (c, o)) next offset in
  let r = parse "hello" p in
  Alcotest.(check (result (pair char int) string) "'h',1" (Ok ('h', 1)) r)

let next_test_suite = [("next is ('h', 1)", `Quick, test_next)]

(* Return. *)

let test_return_int () =
  let r = parse "" (return 5) in
  Alcotest.(check (result int string) "5" (Ok 5) r)

let test_return_string () =
  let r = parse "" (return "hello") in
  Alcotest.(check (result string string) "hello" (Ok "hello") r)

let others_test_suite =
  [ ("return : 5", `Quick, test_return_int)
  ; ("return : \"hello\"", `Quick, test_return_string) ]

let () =
  Printexc.record_backtrace true ;
  Alcotest.run
    "reparse"
    [ ("line/column number", line_column_number_test_suite)
    ; ("peek_char", peek_char_test_suite)
    ; ("next", next_test_suite)
    ; ("return", others_test_suite)
    ; (">>=", Bind.suite) ]
