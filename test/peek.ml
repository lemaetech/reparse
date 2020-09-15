open Reparse

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

let suite =
  [ ("char : 'h'", `Quick, test_peek_char_h)
  ; ("offset :0 0", `Quick, test_peek_char_offset)
  ; ("many/offset : 0", `Quick, test_peek_char_many) ]
