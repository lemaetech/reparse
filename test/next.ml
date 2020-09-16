open Reparse

let test_next () =
  let p = map2 (fun c o -> (c, o)) next offset in
  let r = parse "hello" p in
  Alcotest.(check (pair char int) "'h',1" ('h', 1) r)

let suite = [("next is ('h', 1)", `Quick, test_next)]
