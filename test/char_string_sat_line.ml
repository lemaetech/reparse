module P = Reparse.Parser

let char_h input () =
  let p = P.map2 (fun c o -> (c, o)) (P.char 'h') P.offset in
  let r = P.parse input p in
  Alcotest.(check (pair char int) "'h', 1" ('h', 1) r)

let char_exn input () =
  let p = P.char 'h' in
  let r () = ignore (P.parse input p) in
  Alcotest.(
    check_raises "char"
      (P.Parser
         { offset= 0
         ; line_number= 0
         ; column_number= 0
         ; msg= "[char] expected 'h'" })
      r)

let string input () =
  let p = P.map2 (fun s o -> (s, o)) (P.string "hello") P.offset in
  let r = P.parse input p in
  Alcotest.(check (pair string int) "\"hello\", 4" ("hello", 5) r)

let string_exn input () =
  let p = P.string "hello" in
  let r () = ignore (P.parse input p) in
  Alcotest.(
    check_raises "string"
      (P.Parser {offset= 0; line_number= 0; column_number= 0; msg= "[string]"})
      r)

let is_char = function 'a' | 'b' | 'c' -> true | _ -> false

let char_if input () =
  let p = P.char_if is_char in
  let r = P.parse input p in
  Alcotest.(check char "a" 'a' r)

let char_if_exn input () =
  let p = P.char_if is_char in
  let r () = ignore (P.parse input p) in
  Alcotest.(
    check_raises "char_if"
      (P.Parser {offset= 0; line_number= 0; column_number= 0; msg= "[char_if]"})
      r)

let line_lf input () =
  let p = P.take (P.line `LF) in
  let r = P.parse input p in
  Alcotest.(check (list string) "3, lines" ["abc"; "def"; "ghi"] r)

let line_crlf input () =
  let p = P.take (P.line `CRLF) in
  let r = P.parse input p in
  Alcotest.(check (list string) "3, lines" ["abc"; "def"; "ghi"] r)

module M = Make_test

let suite =
  [ M.make "char " char_h "hello"; M.make "char exn" char_exn "aaaa"
  ; M.make "string" string "hello"; M.make "string exn" string_exn "world"
  ; M.make "char_if" char_if "a"; M.make "char_if exn" char_if_exn "d"
  ; M.make "line lf" line_lf "abc\ndef\nghi"
  ; M.make "line crlf" line_crlf "abc\r\ndef\r\nghi" ]
  |> List.concat
