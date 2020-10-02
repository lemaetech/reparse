module type S = Reparse.Parse_sig.S

type 's test =
  (module Reparse.Parse_sig.S with type src = 's) -> 's -> unit -> unit

let () = Random.self_init ()

let make_string_test test_name (test : 's test) test_data =
  let src = Reparse.Source.String.create test_data in
  ( "[String] " ^ test_name
  , `Quick
  , test (module Reparse.Parser.String_parser) src )

let make_file_test test_name (test : 's test) test_data =
  let make_file content =
    let fname = string_of_int @@ Random.bits () in
    let fd = Unix.openfile fname [Unix.O_RDWR; Unix.O_CREAT] 0o640 in
    let _w = Unix.write_substring fd content 0 (String.length content) in
    fd
  in
  let src = make_file test_data |> Reparse.Source.File.create in
  ("[File]   " ^ test_name, `Quick, test (module Reparse.Parser.File_parser) src)
