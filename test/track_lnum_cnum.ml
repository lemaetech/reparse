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

let suite =
  [ ("lnum/cnum : 1/11", `Quick, test_lnum_cnum_is_1_11)
  ; ("lnum/cnum : 3/0", `Quick, test_lnum_cnum_is_3_0)
  ; ("lnum/cnum : 3/5", `Quick, test_lnum_cnum_is_3_5)
  ; ("lnum/cnum : 0/0", `Quick, test_lnum_cnum_is_0_0) ]
