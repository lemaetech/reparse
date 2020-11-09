module type S = Reparse.Parser_sig.S

type 's test =
  (module Reparse.Parser_sig.S with type input = 's) -> 's -> unit -> unit

let () = Random.self_init ()

let make_string_test test_name (test : 's test) test_data =
  let input = Reparse.Input.String.create test_data in
  ("[String] " ^ test_name, `Quick, test (module Reparse.String_parser) input)

let make_file_test test_name (test : 's test) test_data =
  let make_file content =
    let fname = string_of_int @@ Random.bits () in
    let fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT] 0o640 in
    let _w = Unix.write_substring fd content 0 (String.length content) in
    fd
  in
  let input = make_file test_data |> Reparse.Input.File.create in
  ("[File]   " ^ test_name, `Quick, test (module Reparse.File_parser) input)
