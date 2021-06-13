module Make_test (P : Test_parser.TEST_PARSER) = struct
  type int_result = (int, string) result [@@deriving show, ord, popper]

  type string_result = (string, string) result [@@deriving show, ord, popper]

  open P.Infix

  let pos_test p pos inp =
    ( "pos"
    , Popper.(
        test (fun () ->
            let p = p *> P.pos in
            equal int_result_comparator (P.run p inp) (Ok pos))) )

  let committed_pos_test p pos inp =
    ( "committed_pos"
    , Popper.(
        test (fun () ->
            let p = p *> P.committed_pos in
            equal int_result_comparator (P.run p inp) (Ok pos))) )

  let peek_char =
    let p = P.peek_char >>= fun c -> P.string_of_chars [ c ] in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( "value"
          , test (fun () ->
                equal string_result_comparator (P.run p @@ inp ()) (Ok "h")) )
        ; pos_test p 0 @@ inp ()
        ; committed_pos_test p 0 @@ inp ()
        ])

  let take_string =
    let p = P.take_string 5 in
    let inp () = P.of_string "hello world" in
    Popper.(
      suite
        [ ( "value"
          , test (fun () ->
                equal string_result_comparator (P.run p @@ inp ()) (Ok "hello"))
          )
        ; pos_test p 5 @@ inp ()
        ; committed_pos_test p 0 @@ inp ()
        ])

  let suites =
    Popper.suite [ ("take_string", take_string); ("peek_char", peek_char) ]
end

let suite =
  let module S = Make_test (Test_parser.String) in
  let module L = Make_test (Test_parser.Lwt) in
  Popper.suite [ ("Reparse.String", S.suites); ("Lwt.Stream", S.suites) ]
