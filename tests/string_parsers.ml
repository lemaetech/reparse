module Make_test (P : Test_parser.TEST_PARSER) = struct
  type int_result = (int, string) result [@@deriving show, ord, popper]

  type string_result = (string, string) result [@@deriving show, ord, popper]

  open P.Infix

  let take_string =
    let p = P.take_string 5 in
    let inp () = P.of_string "hello world" in

    Popper.(
      suite
        [ ( "value"
          , test (fun () ->
                equal string_result_comparator (P.run p @@ inp ()) (Ok "hello"))
          )
        ; ( "pos"
          , test (fun () ->
                let p = p *> P.pos in
                equal int_result_comparator (P.run p @@ inp ()) (Ok 5)) )
        ; ( "committed_pos"
          , test (fun () ->
                let p = p *> P.committed_pos in
                equal int_result_comparator (P.run p @@ inp ()) (Ok 0)) )
        ])

  let suites = Popper.suite [ ("take_string", take_string) ]
end

let suite =
  let module S = Make_test (Test_parser.String) in
  let module L = Make_test (Test_parser.Lwt) in
  Popper.suite [ ("Reparse.String", S.suites); ("Lwt.Stream", S.suites) ]
