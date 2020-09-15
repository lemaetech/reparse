open Reparse

let test_next () =
  let p = map2 (fun c o -> (c, o)) next offset in
  let r = parse "hello" p in
  Alcotest.(check (result (pair char int) string) "'h',1" (Ok ('h', 1)) r)

let suite = [("next is ('h', 1)", `Quick, test_next)]
