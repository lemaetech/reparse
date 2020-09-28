module P = Reparse.Parse.String_parser

let src = Reparse.IO.String.create

let char_h () =
  let p = P.map2 (fun c o -> (c, o)) (P.char 'h') P.offset in
  let r = P.parse (src "hello") p in
  Alcotest.(check (pair char int) "'h', 1" ('h', 1) r)

let char_exn () =
  let p = P.char 'h' in
  let r () = ignore (P.parse (src "aaaa") p) in
  Alcotest.(
    check_raises
      "char"
      (P.Parse_error
         { offset = 0
         ; line_number = 0
         ; column_number = 0
         ; msg = "[char] expected 'h'" })
      r)

let string () =
  let p = P.map2 (fun s o -> (s, o)) (P.string "hello") P.offset in
  let r = P.parse (src "hello") p in
  Alcotest.(check (pair string int) "\"hello\", 4" ("hello", 5) r)

let string_exn () =
  let p = P.string "hello" in
  let r () = ignore (P.parse (src "world") p) in
  Alcotest.(
    check_raises
      "string"
      (P.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[string]"})
      r)

let is_char = function
  | 'a'
  | 'b'
  | 'c' ->
      true
  | _ -> false

let satisfy () =
  let p = P.satisfy is_char in
  let r = P.parse (src "a") p in
  Alcotest.(check char "a" 'a' r)

let satisfy_exn () =
  let p = P.satisfy is_char in
  let r () = ignore (P.parse (src "d") p) in
  Alcotest.(
    check_raises
      "satisfy"
      (P.Parse_error
         {offset = 0; line_number = 0; column_number = 0; msg = "[satisfy]"})
      r)

let suite =
  [ ("char ", `Quick, char_h)
  ; ("char exn", `Quick, char_exn)
  ; ("string", `Quick, string)
  ; ("string exn", `Quick, string_exn)
  ; ("satisfy", `Quick, satisfy)
  ; ("satisfy exn", `Quick, satisfy_exn) ]
