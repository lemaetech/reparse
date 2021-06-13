module Make_test (P : Helper.TEST_PARSER) = struct
  type int_result = (int, string) result [@@deriving show, ord, popper]

  type string_result = (string, string) result [@@deriving show, ord, popper]

  let take_string =
    let p = P.take_string 5 in
    let inp () = P.of_string "hello world" in

    Popper.(
      suite
        [ ( "value"
          , test (fun () ->
                equal string_result_comparator (P.run p (inp ())) (Ok "hello"))
          )
        ; ( "pos"
          , test (fun () ->
                equal int_result_comparator
                  (P.(run (p *> P.pos)) (inp ()))
                  (Ok 5)) )
        ])

  let suites = Popper.suite [ ("take_string", take_string) ]
end

let suite =
  let module S = Make_test (Helper.String_parser_tester) in
  let module L = Make_test (Helper.Lwt_parser_tester) in
  Popper.suite [ ("Reparse.String", S.suites); ("Lwt.Stream", S.suites) ]
