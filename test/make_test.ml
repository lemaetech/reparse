module P = Reparse.Parser

type test' = P.input -> unit -> unit

let make test_name test data =
  let fd =
    let fname = string_of_int @@ Random.bits () in
    let fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT] 0o640 in
    let _w = Unix.write_substring fd data 0 (String.length data) in
    fd in
  let s_input = new P.string_input data in
  let fd_input = new P.file_input fd in
  [ ("[String] " ^ test_name, `Quick, test s_input)
  ; ("[File]   " ^ test_name, `Quick, test fd_input) ]
