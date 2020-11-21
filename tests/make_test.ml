module P = Reparse.Parser

let parse_string input p = P.parse_string p input
let parse input p = P.parse p input

let make test_name test data =
  let fd =
    let fname = string_of_int @@ Random.bits () in
    let fd = Unix.openfile fname [ Unix.O_RDWR; Unix.O_CREAT ] 0o640 in
    let _w = Unix.write_substring fd data 0 (String.length data) in
    fd
  in
  let file_input = Reparse_unix.File_input.create fd in
  [ "[S] - " ^ test_name, `Quick, test (parse_string data)
  ; "[F] - " ^ test_name, `Quick, test (parse file_input)
  ]
;;
