module P = Reparse.Parser.String_parser
open P.Infix

let src = Reparse.Source.String.create

let test_lnum_cnum_1 () =
  let p =
    P.skip P.next *> P.map2 (fun lnum cnum -> (lnum, cnum)) P.lnum P.cnum
  in
  let r = P.parse ~track_lnum:true (src "hello world") p in
  Alcotest.(check (pair int int) "1/12" (1, 12) r)

let test_lnum_cnum_2 () =
  let p =
    P.skip P.next *> P.map2 (fun lnum cnum -> (lnum, cnum)) P.lnum P.cnum
  in
  let r = P.parse ~track_lnum:true (src "hello world\nsecond line\n") p in
  Alcotest.(check (pair int int) "3/1" (3, 1) r)

let test_lnum_cnum_3 () =
  let p =
    P.skip P.next *> P.map2 (fun lnum cnum -> (lnum, cnum)) P.lnum P.cnum
  in
  let r = P.parse ~track_lnum:true (src "hello world\nsecond line\naaaaa") p in
  Alcotest.(check (pair int int) "3/5" (3, 6) r)

let test_lnum_cnum_4 () =
  let p =
    P.skip P.next *> P.map2 (fun lnum cnum -> (lnum, cnum)) P.lnum P.cnum
  in
  let r = P.parse (src "hello world\nsecond line\naaaaa") p in
  Alcotest.(check (pair int int) "0/0" (0, 0) r) ;

  let p =
    P.skip P.next *> P.map2 (fun lnum cnum -> (lnum, cnum)) P.lnum P.cnum
  in
  let r = P.parse ~track_lnum:false (src "hello world\nsecond line\naaaaa") p in
  Alcotest.(check (pair int int) "0/0" (0, 0) r)

let suite =
  [ ("lnum/cnum : 1/12", `Quick, test_lnum_cnum_1)
  ; ("lnum/cnum : 3/1", `Quick, test_lnum_cnum_2)
  ; ("lnum/cnum : 3/5", `Quick, test_lnum_cnum_3)
  ; ("lnum/cnum : 0/0", `Quick, test_lnum_cnum_4) ]
