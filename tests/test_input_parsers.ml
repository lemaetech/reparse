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
        [ ( "value is ()"
          , test (fun () -> equal unit_result_comparator (P.run p inp) (Ok ()))
          )
        ; pos_test p 11 inp
        ; last_trimmed_pos_test p 11 inp
        ])

  let suites = Popper.suite [ ("trim_input_buffer", trim_input_buffer) ]
end

let suite =
  let module S = Make_test (Test_parser.String) in
  let module L = Make_test (Test_parser.Lwt) in
  Popper.suite [ ("String", S.suites); ("Lwt.Stream", L.suites) ]
