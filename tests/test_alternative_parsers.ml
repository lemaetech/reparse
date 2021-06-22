module Make_test (P : Test_parser.TEST_PARSER) = struct
  open Test_parser.Make_helper (P)

  open P.Infix

  let any =
    let p = P.(any [ string_cs "c"; string_cs "h" ]) in
    let p2 = P.(any [ string_cs "hello"; take_string 11 ]) in
    let p3 = P.(any [ take_string 2; string_cs "hello" ]) in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [
          ( {|value is "h"|},
            test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "h")) );
          pos_test p 1 inp;
          last_trimmed_pos_test p 0 inp;
          ( "fail",
            test (fun () ->
                let p = P.(any [ string_cs "a"; string_cs "b" ]) in
                equal string_result_comparator (P.run p inp)
                  (Error "[any] all parsers failed")) );
          ( {|first success value is "hello"|},
            test (fun () ->
                equal string_result_comparator (P.run p2 inp) (Ok "hello")) );
          pos_test p2 5 inp;
          last_trimmed_pos_test p2 0 inp;
          ( {|first success value is "he"|},
            test (fun () ->
                equal string_result_comparator (P.run p3 inp) (Ok "he")) );
          pos_test p3 2 inp;
          last_trimmed_pos_test p3 0 inp;
        ])

  let alt =
    let p = P.(alt (string_cs "world") (string_cs "hello")) in
    let p2 = P.(alt (string_cs "world") (string_cs "how")) in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [
          ( "value is hello",
            test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "hello")) );
          pos_test p 5 inp;
          last_trimmed_pos_test p 0 inp;
          ( "fail",
            test (fun () ->
                equal string_result_comparator (P.run p2 inp)
                  (Error {|[string_cs] "how"|})) );
          ( "pos is fail",
            test (fun () ->
                let p = p2 *> P.pos in
                equal int_result_comparator (P.run p inp)
                  (Error {|[string_cs] "how"|})) );
          ( "last_trimmed_pos is fail",
            test (fun () ->
                let p = p2 *> P.last_trimmed_pos in
                equal int_result_comparator (P.run p inp)
                  (Error {|[string_cs] "how"|})) );
        ])

  let optional =
    let p = P.(optional (string_cs "hello")) in
    let inp () = P.of_string "hello" in
    let inp2 () = P.of_string "world" in
    Popper.(
      suite
        [
          ( {|value is 'Some "hello"']|},
            test (fun () ->
                equal string_opt_result_comparator (P.run p inp)
                  (Ok (Some "hello"))) );
          pos_test p 5 inp;
          last_trimmed_pos_test p 0 inp;
          ( {|value is 'None'|},
            test (fun () ->
                equal string_opt_result_comparator (P.run p inp2) (Ok None)) );
          pos_test p 0 inp2;
          last_trimmed_pos_test p 0 inp2;
          ( {|value when eof is 'None'|},
            test (fun () ->
                equal string_opt_result_comparator (P.run p empty) (Ok None)) );
        ])

  let suites =
    Popper.suite [ ("any", any); ("alt", alt); ("optional", optional) ]
end

let suite =
  let module S = Make_test (Test_parser.String) in
  let module L = Make_test (Test_parser.Lwt) in
  Popper.suite [ ("String", S.suites); ("Lwt.Stream", L.suites) ]
