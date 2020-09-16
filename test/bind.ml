open Reparse

let test_bind () =
  let p = char 'h' >>= fun c -> return (Char.code c) in
  let r = parse "hello" p in
  Alcotest.(check int "104" 104 r)

let suite = [(">>= ", `Quick, test_bind)]
