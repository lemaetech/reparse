module Make_test (P : Test_parser.TEST_PARSER) = struct
  open Test_parser.Make_helper (P)

  open P.Infix

  let not_ =
    let p = P.(not_ (string_cs "hello")) in
    let p2 = P.(not_ (string_cs "world")) in
    let inp () = P.of_string "world" in
    Popper.(
      suite
        [ ( "value is ()"
          , test (fun () -> equal unit_result_comparator (P.run p inp) (Ok ()))
          )
        ; pos_test p 0 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "fail"
          , test (fun () ->
                equal unit_result_comparator (P.run p2 inp)
                  (Error "[not_] expected failure but succeeded") ) )
        ; ( "pos is error"
          , test (fun () ->
                let p = p2 *> P.pos in
                equal int_result_comparator (P.run p inp)
                  (Error "[not_] expected failure but succeeded") ) ) ])

  let is =
    let p = P.(is (string_cs "hello")) in
    let p2 = P.(is (string_cs "world")) in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( "value is true"
          , test (fun () ->
                equal bool_result_comparator (P.run p inp) (Ok true) ) )
        ; pos_test p 0 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "value is false"
          , test (fun () ->
                equal bool_result_comparator (P.run p2 inp) (Ok false) ) )
        ; pos_test p2 0 inp
        ; last_trimmed_pos_test p2 0 inp ])

  let is_not =
    let p = P.(is_not (string_cs "hello")) in
    let p2 = P.(is_not (string_cs "world")) in
    let inp () = P.of_string "world" in
    Popper.(
      suite
        [ ( "value is true"
          , test (fun () ->
                equal bool_result_comparator (P.run p inp) (Ok true) ) )
        ; pos_test p 0 inp
        ; last_trimmed_pos_test p 0 inp
        ; ( "value is false"
          , test (fun () ->
                equal bool_result_comparator (P.run p2 inp) (Ok false) ) )
        ; pos_test p2 0 inp
        ; last_trimmed_pos_test p2 0 inp ])

  let suites = Popper.suite [("not_", not_); ("is", is); ("is_not", is_not)]
end

let suite =
  let module String = Make_test (Test_parser.String) in
  let module Lwt_stream = Make_test (Test_parser.Lwt_stream) in
  let module Lwt_fd = Make_test (Test_parser.Lwt_fd) in
  let module Lwt_channel = Make_test (Test_parser.Lwt_channel) in
  Popper.suite
    [ ("Reparse.String", String.suites)
    ; ("Reparse_lwt.Stream", Lwt_stream.suites)
    ; ("Reparse_lwt_unix.Fd", Lwt_fd.suites)
    ; ("Reparse_lwt_unix.Channel", Lwt_channel.suites) ]
