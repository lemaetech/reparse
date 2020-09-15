open Reparse

let test_lnum_cnum_is_1_11 () =
  let p = many next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum in
  let r = parse ~track_lnum:true "hello world" p in
  Alcotest.(
    check (result (pair int int) string) "line number: 1" (Ok (1, 11)) r)

let test_lnum_cnum_is_3_0 () =
  let p = many next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum in
  let r = parse ~track_lnum:true "hello world\nsecond line\n" p in
  Alcotest.(check (result (pair int int) string) "line number: 2" (Ok (3, 0)) r)

let test_lnum_cnum_is_3_5 () =
  let p = many next *> map2 (fun lnum cnum -> (lnum, cnum)) lnum cnum in
  let r = parse ~track_lnum:true "hello world\nsecond line\naaaaa" p in
  Alcotest.(check (result (pair int int) string) "line number: 2" (Ok (3, 5)) r)

let test_lnum_cnum_suite =
  [ ("test (lnum, cnum) is (1,11)", `Quick, test_lnum_cnum_is_1_11)
  ; ("test (lnum, cnum) is (3, 0)", `Quick, test_lnum_cnum_is_3_0)
  ; ("test (lnum, cnum) is (3, 5)", `Quick, test_lnum_cnum_is_3_5) ]

let () =
  Printexc.record_backtrace true ;
  Alcotest.run "reparse" [("(lnum,cnum)", test_lnum_cnum_suite)]
