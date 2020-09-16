module R = Reparse

let char_h () =
  let p = R.map2 (fun c o -> (c, o)) (R.char 'h') R.offset in
  let r = R.parse "hello" p in
  Alcotest.(check (pair unit int) "'h', 1" ((), 1) r)

let string_hello () =
  let p = R.map2 (fun s o -> (s, o)) (R.string "hello") R.offset in
  let r = R.parse "hello" p in
  Alcotest.(check (pair unit int) "\"hello\", 4" ((), 4) r)

let suite = [("char 'h'", `Quick, char_h)]
