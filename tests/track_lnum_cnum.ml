module P = Reparse.Parser
open P.Infix

let test_lnum_cnum_1 () =
  let p = P.skip P.next *> P.map2 (fun lnum cnum -> lnum, cnum) P.lnum P.cnum in
  let r = P.parse_string ~track_lnum:true p "hello world" in
  Alcotest.(check (pair int int) "1/12" (1, 12) r)
;;

let test_lnum_cnum_2 () =
  let p = P.skip P.next *> P.map2 (fun lnum cnum -> lnum, cnum) P.lnum P.cnum in
  let r = P.parse_string ~track_lnum:true p "hello world\nsecond line\n" in
  Alcotest.(check (pair int int) "3/1" (3, 1) r)
;;

let test_lnum_cnum_3 () =
  let p = P.skip P.next *> P.map2 (fun lnum cnum -> lnum, cnum) P.lnum P.cnum in
  let r = P.parse_string ~track_lnum:true p "hello world\nsecond line\naaaaa" in
  Alcotest.(check (pair int int) "3/5" (3, 6) r)
;;

let test_lnum_cnum_4 () =
  let p = P.skip P.next *> P.map2 (fun lnum cnum -> lnum, cnum) P.lnum P.cnum in
  let r = P.parse_string p "hello world\nsecond line\naaaaa" in
  Alcotest.(check (pair int int) "0/0" (0, 0) r);
  let p = P.skip P.next *> P.map2 (fun lnum cnum -> lnum, cnum) P.lnum P.cnum in
  let r = P.parse_string ~track_lnum:false p "hello world\nsecond line\naaaaa" in
  Alcotest.(check (pair int int) "0/0" (0, 0) r)
;;

let suite =
  [ "lnum/cnum : 1/12", `Quick, test_lnum_cnum_1
  ; "lnum/cnum : 3/1", `Quick, test_lnum_cnum_2
  ; "lnum/cnum : 3/5", `Quick, test_lnum_cnum_3
  ; "lnum/cnum : 0/0", `Quick, test_lnum_cnum_4
  ]
;;
