open Reparse.Infix
module R = Reparse

let test_lnum_cnum_1 () =
  let p =
    R.many R.next *> R.map2 (fun lnum cnum -> (lnum, cnum)) R.lnum R.cnum
  in
  let r = R.parse ~track_lnum:true "hello world" p in
  Alcotest.(check (pair int int) "1/12" (1, 12) r)

let test_lnum_cnum_2 () =
  let p =
    R.many R.next *> R.map2 (fun lnum cnum -> (lnum, cnum)) R.lnum R.cnum
  in
  let r = R.parse ~track_lnum:true "hello world\nsecond line\n" p in
  Alcotest.(check (pair int int) "3/1" (3, 1) r)

let test_lnum_cnum_3 () =
  let p =
    R.many R.next *> R.map2 (fun lnum cnum -> (lnum, cnum)) R.lnum R.cnum
  in
  let r = R.parse ~track_lnum:true "hello world\nsecond line\naaaaa" p in
  Alcotest.(check (pair int int) "3/6" (3, 6) r)

let test_lnum_cnum_4 () =
  let p =
    R.many R.next *> R.map2 (fun lnum cnum -> (lnum, cnum)) R.lnum R.cnum
  in
  let r = R.parse "hello world\nsecond line\naaaaa" p in
  Alcotest.(check (pair int int) "0/0" (0, 0) r) ;

  let p =
    R.many R.next *> R.map2 (fun lnum cnum -> (lnum, cnum)) R.lnum R.cnum
  in
  let r = R.parse ~track_lnum:false "hello world\nsecond line\naaaaa" p in
  Alcotest.(check (pair int int) "0/0" (0, 0) r)

let advance () =
  let p =
    R.advance 6 *> R.map3 (fun o l c -> [o; l; c]) R.offset R.lnum R.cnum
  in
  let r = R.parse ~track_lnum:true "hello\nworld" p in
  Alcotest.(check (list int) "[6; 1;0] " [6; 2; 1] r)

let suite =
  [ ("lnum/cnum : 1/12", `Quick, test_lnum_cnum_1)
  ; ("lnum/cnum : 3/1", `Quick, test_lnum_cnum_2)
  ; ("lnum/cnum : 3/6", `Quick, test_lnum_cnum_3)
  ; ("lnum/cnum : 0/0", `Quick, test_lnum_cnum_4)
  ; ("advance: offset:6 lnum:2 cnum:1", `Quick, advance) ]
