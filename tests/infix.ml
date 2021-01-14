module P = Reparse.Parser
open P

let test_map () =
  let p = P.peek_char >>| fun c -> Char.code c in
  let r = P.parse_string p "hello" in
  Alcotest.(check int "104" 104 r)
;;

let test_bind () =
  let p = P.peek_char >>= fun c -> P.return (Char.code c) in
  let r = P.parse_string p "hello" in
  Alcotest.(check int "104" 104 r)
;;

let test_applicative () =
  let p = P.return (fun a -> a + 2) <*> P.return 2 in
  let r = P.parse_string p "" in
  Alcotest.(check int "4" 4 r)
;;

let test_less_dollar () =
  let p = 4 <$ P.char 'h' in
  let r = P.parse_string p "hello" in
  Alcotest.(check int "4" 4 r)
;;

let suite =
  [ ">>= ", `Quick, test_bind
  ; ">|= ", `Quick, test_map
  ; "<*>", `Quick, test_applicative
  ; "<$", `Quick, test_less_dollar
  ]
;;
