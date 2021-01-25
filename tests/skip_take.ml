module P = Reparse
open P

let make_pair a b = a, b

let skip parse () =
  let p = P.map2 ~f:make_pair (P.skip P.space) P.offset in
  let r = parse p in
  Alcotest.(check (pair int int) "4, 4" (4, 4) r)
;;

let skip_at_least parse () =
  let p = P.map2 ~f:make_pair (P.skip ~at_least:3 P.space) P.offset in
  let r = parse p in
  Alcotest.(check (pair int int) "4, 4" (4, 4) r)
;;

let skip_at_least_fail parse () =
  let p = P.map2 ~f:make_pair (P.skip ~at_least:5 P.space) P.offset in
  let r () = ignore (parse p) in
  Alcotest.(
    check_raises
      "skip ~at_least fails"
      (P.Parser
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[skip] unable to parse at_least 5 times"
         })
      r)
;;

let skip_upto parse () =
  let p = P.map2 ~f:make_pair (P.skip ~up_to:3 P.space) P.offset in
  let r = parse p in
  Alcotest.(check (pair int int) "3, 3" (3, 3) r)
;;

let skip_skip_skip parse () =
  let p = P.map2 ~f:make_pair (P.skip (P.skip (P.skip P.space))) P.offset in
  let r = parse p in
  Alcotest.(check (pair int int) "1, 5" (1, 5) r)
;;

let skip_while parse () =
  let p =
    P.map2
      ~f:make_pair
      (P.skip_while (P.next *> unit) ~while_:(P.is_not (P.char 'z')))
      P.peek_char
  in
  let r = parse p in
  Alcotest.(check (pair int char) "4, z" (4, 'z') r)
;;

let skip_while2 parse () =
  let p =
    P.map2
      ~f:make_pair
      (P.skip_while (P.char 'a' *> unit) ~while_:(P.is_not (P.char 'z')))
      P.peek_char
  in
  let r = parse p in
  Alcotest.(check (pair int char) "4, c" (4, 'c') r)
;;

let take parse () =
  let p = P.map2 ~f:make_pair (P.take (P.char 'a')) P.offset in
  let r = parse p in
  Alcotest.(check (pair (list char) int) "" ([ 'a'; 'a'; 'a'; 'a' ], 4) r)
;;

let take_at_least parse () =
  let p = P.map2 ~f:make_pair (P.take ~at_least:4 (P.char 'a')) P.offset in
  let r = parse p in
  Alcotest.(check (pair (list char) int) "" ([ 'a'; 'a'; 'a'; 'a' ], 4) r)
;;

let take_at_least_fail parse () =
  let p = P.map2 ~f:make_pair (P.take ~at_least:5 (P.char 'a')) P.offset in
  let r () = ignore (parse p) in
  Alcotest.(
    check_raises
      "skip ~at_least fails"
      (P.Parser
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[take] unable to parse at least 5 times"
         })
      r)
;;

let take_up_to parse () =
  let p = P.map2 ~f:make_pair (P.take ~up_to:3 (P.char 'a')) P.offset in
  let r = parse p in
  Alcotest.(check (pair (list char) int) "" ([ 'a'; 'a'; 'a' ], 3) r)
;;

let take_sep_by parse () =
  let p = P.map2 ~f:make_pair (P.take ~sep_by:P.space (P.char 'a')) P.offset in
  let r = parse p in
  Alcotest.(check (pair (list char) int) "" ([ 'a'; 'a'; 'a'; 'a' ], 7) r)
;;

let take_at_least_up_to_sep_by parse () =
  let p =
    P.map2
      ~f:make_pair
      (P.take ~at_least:3 ~up_to:3 ~sep_by:P.space (P.char 'a'))
      P.offset
  in
  let r = parse p in
  Alcotest.(check (pair (list char) int) "" ([ 'a'; 'a'; 'a' ], 6) r)
;;

let take_while parse () =
  let p =
    P.map2
      ~f:make_pair
      (P.take_while (P.char 'a') ~while_:(P.is_not (P.char 'z')))
      P.offset
  in
  let r = parse p in
  Alcotest.(check (pair (list char) int) "" ([ 'a'; 'a'; 'a'; 'a' ], 4) r)
;;

let take_while_sep parse () =
  let p =
    P.map2
      ~f:make_pair
      (P.take_while
         ~sep_by:(P.space *> unit)
         (P.char 'a')
         ~while_:(P.is_not (P.char 'z')))
      P.offset
  in
  let r = parse p in
  Alcotest.(check (pair (list char) int) "" ([ 'a'; 'a'; 'a'; 'a' ], 8) r)
;;

let take_while_cb parse () =
  let buf = Buffer.create 5 in
  let p =
    P.map2
      ~f:make_pair
      (P.take_while_cb
         (P.char 'a')
         ~while_:(P.is_not (P.char 'z'))
         ~on_take_cb:(fun c -> Buffer.add_char buf c))
      P.offset
  in
  let r = parse p in
  Alcotest.(
    check (pair (pair int int) string) "" ((4, 4), "aaaa") (r, Buffer.contents buf))
;;

let take_while_cb_sep_by parse () =
  let buf = Buffer.create 5 in
  let p =
    P.map2
      ~f:make_pair
      (P.take_while_cb
         (P.char 'a')
         ~while_:(P.is_not (P.char 'z'))
         ~sep_by:(ignore_m P.space)
         ~on_take_cb:(fun c -> Buffer.add_char buf c))
      P.offset
  in
  let r = parse p in
  Alcotest.(
    check (pair (pair int int) string) "" ((4, 8), "aaaa") (r, Buffer.contents buf))
;;

let take_between parse () =
  let p =
    P.(
      take_between
        ~sep_by:(char ',' *> unit)
        ~start:(P.char '(' *> unit)
        ~end_:(char ')' *> unit)
        next)
  in
  let r = parse p in
  Alcotest.(check (list char) "" [ 'a'; 'a'; 'a' ] r)
;;

module M = Make_test

let suite =
  [ M.make "skip" skip "    a"
  ; M.make "skip ~at_least:3" skip_at_least "    a"
  ; M.make "skip ~at_least:5 fails" skip_at_least_fail "    a"
  ; M.make "skip ~up_to:3" skip_upto "     a"
  ; M.make "skip skip skip" skip_skip_skip "     a"
  ; M.make "skip while" skip_while "aaaaz"
  ; M.make "skip while" skip_while2 "aaaacz"
  ; M.make "take" take "aaaacz"
  ; M.make "take at least" take_at_least "aaaacz"
  ; M.make "take at least fail" take_at_least_fail "aaaacz"
  ; M.make "take up_to" take_up_to "aaaacz"
  ; M.make "take sep_by" take_sep_by "a a a acz"
  ; M.make "take at_least up_to sep_by" take_at_least_up_to_sep_by "a a a acz"
  ; M.make "take_while" take_while "aaaacz"
  ; M.make "take_while sep_by" take_while_sep "a a a a cz"
  ; M.make "take take_while" take_while "aaaacz"
  ; M.make "take_while_cb" take_while_cb "aaaacz"
  ; M.make "take_while_cb sepby" take_while_cb_sep_by "a a a a cz"
  ; M.make "take_between" take_between "(a,a,a)"
  ]
  |> List.concat
;;
