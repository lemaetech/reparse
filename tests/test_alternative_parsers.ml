module Make_test (P : Test_parser.TEST_PARSER) = struct
  open Test_parser.Make_helper (P)

  open P.Infix

  let any =
    let p = P.(any [ string "c"; string "h" ]) in
    let p2 = P.(any [ string "hello"; take_string 11 ]) in
    let p3 = P.(any [ take_string 2; string "hello" ]) in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( {|value is "h"|}
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "h")) )
        ; pos_test p 1 inp
        ; committed_pos_test p 0 inp
        ; ( "fail"
          , test (fun () ->
                let p = P.(any [ string "a"; string "b" ]) in
                equal string_result_comparator (P.run p inp)
                  (Error "[any] all parsers failed")) )
        ; ( {|first success value is "hello"|}
          , test (fun () ->
                equal string_result_comparator (P.run p2 inp) (Ok "hello")) )
        ; pos_test p2 5 inp
        ; committed_pos_test p2 0 inp
        ; ( {|first success value is "he"|}
          , test (fun () ->
                equal string_result_comparator (P.run p3 inp) (Ok "he")) )
        ; pos_test p3 2 inp
        ; committed_pos_test p3 0 inp
        ])

  let alt =
    let p = P.(alt (string "world") (string "hello")) in
    let p2 = P.(alt (string "world") (string "how")) in
    let inp () = P.of_string "hello" in
    Popper.(
      suite
        [ ( "value is hello"
          , test (fun () ->
                equal string_result_comparator (P.run p inp) (Ok "hello")) )
        ; pos_test p 5 inp
        ; committed_pos_test p 0 inp
        ; ( "fail"
          , test (fun () ->
                equal string_result_comparator (P.run p2 inp)
                  (Error {|[string] "how"|})) )
        ; ( "pos is fail"
          , test (fun () ->
                let p = p2 *> P.pos in
                equal int_result_comparator (P.run p inp)
                  (Error {|[string] "how"|})) )
        ; ( "committed_pos is fail"
          , test (fun () ->
                let p = p2 *> P.committed_pos in
                equal int_result_comparator (P.run p inp)
                  (Error {|[string] "how"|})) )
        ])

  let optional =
    let p = P.(optional (string "hello")) in
    let inp () = P.of_string "hello" in
    let inp2 () = P.of_string "world" in
    Popper.(
      suite
        [ ( {|value is 'Some "hello"']|}
          , test (fun () ->
                equal string_opt_result_comparator (P.run p inp)
                  (Ok (Some "hello"))) )
        ; pos_test p 5 inp
        ; committed_pos_test p 0 inp
        ; ( {|value is 'None'|}
          , test (fun () ->
                equal string_opt_result_comparator (P.run p inp2) (Ok None)) )
        ; pos_test p 0 inp2
        ; committed_pos_test p 0 inp2
        ; ( {|value when eof is 'None'|}
          , test (fun () ->
                equal string_opt_result_comparator (P.run p empty) (Ok None)) )
        ])

  let suites =
    Popper.suite [ ("any", any); ("alt", alt); ("optional", optional) ]
end

let suite =
  let module S = Make_test (Test_parser.String) in
  let module L = Make_test (Test_parser.Lwt) in
  Popper.suite [ ("Reparse.String", S.suites); ("Lwt.Stream", S.suites) ]
