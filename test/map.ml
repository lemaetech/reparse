open Reparse

let test_map () =
  let p = char 'h' >|= fun c -> Char.code c in
  let r = parse "hello" p in
  Alcotest.(check (result int string) "104" (Ok 104) r)

let suite = [(">|= ", `Quick, test_map)]
