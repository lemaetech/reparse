module Make_test (P : Test_parser.TEST_PARSER) = struct
  open Test_parser.Make_helper (P)

  let trim_input_buffer =
    let p =
      P.(
        string_cs "hello" *> trim_input_buffer
        <* string_cs " world" *> trim_input_buffer)
    in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [
          ( "value is ()",
            test (fun () -> equal unit_result_comparator (P.run p inp) (Ok ()))
          );
          pos_test p 11 inp;
          last_trimmed_pos_test p 11 inp;
        ])

  let advance =
    let p = P.advance 6 in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [
          ( "value is ()",
            test (fun () -> equal unit_result_comparator (P.run p inp) (Ok ()))
          );
          pos_test p 6 inp;
          last_trimmed_pos_test p 0 inp;
          ( "value is world",
            test (fun () ->
                let p = P.(advance 6 *> string_cs "world") in
                equal string_result_comparator (P.run p inp) (Ok "world")) );
        ])

  let suites =
    Popper.suite
      [ ("trim_input_buffer", trim_input_buffer); ("advance", advance) ]
end

let suite =
  let module String = Make_test (Test_parser.String) in
  let module Lwt_stream = Make_test (Test_parser.Lwt_stream) in
  let module Lwt_fd = Make_test (Test_parser.Lwt_fd) in
  let module Lwt_channel = Make_test (Test_parser.Lwt_channel) in
  Popper.suite
    [
      ("Reparse.String", String.suites);
      ("Reparse_lwt.Stream", Lwt_stream.suites);
      ("Reparse_lwt_unix.Fd", Lwt_fd.suites);
      ("Reparse_lwt_unix.Channel", Lwt_channel.suites);
    ]
